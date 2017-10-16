#include "JIT.h"

#include <cstddef>
#include <mutex>

#include "preprocessor/llvm_includes_start.h"
#include <llvm/IR/Module.h>
#include <llvm/ADT/StringSwitch.h>
#include <llvm/ADT/Triple.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include <llvm/Support/TargetSelect.h>
#include <evm.h>
#include "preprocessor/llvm_includes_end.h"

#include "Ext.h"
#include "Compiler.h"
#include "Optimizer.h"
#include "Cache.h"
#include "ExecStats.h"
#include "Utils.h"
#include "BuildInfo.gen.h"

#include <deps/include/mpir.h>

#define TMP_SDECL
#define TMP_DECL		struct tmp_reentrant_t *__tmp_marker
#define TMP_SMARK
#define TMP_MARK		__tmp_marker = 0
#define TMP_ALLOC(n)	alloca(n)
#define TMP_FREE

#define CNST_LIMB(C) ((mp_limb_t) C##L)
#define GMP_NUMB_HIGHBIT  (CNST_LIMB(1) << (GMP_NUMB_BITS-1))
#define count_leading_zeros(count,x)    \
  do {                                          \
    (count) = __builtin_clzll (x);              \
  } while (0)

struct tmp_reentrant_t {
	struct tmp_reentrant_t  *next;
	size_t		  size;	  /* bytes, including header */
};

/* Allocating various types. */
#define TMP_ALLOC_TYPE(n,type)  ((type *) TMP_ALLOC ((n) * sizeof (type)))
#define TMP_SALLOC_TYPE(n,type) ((type *) TMP_SALLOC ((n) * sizeof (type)))
#define TMP_BALLOC_TYPE(n,type) ((type *) TMP_BALLOC ((n) * sizeof (type)))
#define TMP_ALLOC_LIMBS(n)      TMP_ALLOC_TYPE(n,mp_limb_t)
#define TMP_SALLOC_LIMBS(n)     TMP_SALLOC_TYPE(n,mp_limb_t)
#define TMP_BALLOC_LIMBS(n)     TMP_BALLOC_TYPE(n,mp_limb_t)
#define TMP_ALLOC_MP_PTRS(n)    TMP_ALLOC_TYPE(n,mp_ptr)
#define TMP_SALLOC_MP_PTRS(n)   TMP_SALLOC_TYPE(n,mp_ptr)
#define TMP_BALLOC_MP_PTRS(n)   TMP_BALLOC_TYPE(n,mp_ptr)

#define LIKELY(cond)    __builtin_expect ((cond) != 0, 1)
#define UNLIKELY(cond)  __builtin_expect ((cond) != 0, 0)

/* Copy N limbs from SRC to DST incrementing, N==0 allowed.  */
#if ! defined (MPN_COPY_INCR)
#define MPN_COPY_INCR(dst, src, n)                      \
  do {                                                  \
    if ((n) != 0)                                       \
      {                                                 \
    mp_size_t __n = (n) - 1;                        \
    mp_ptr __dst = (dst);                           \
    mp_srcptr __src = (src);                        \
    mp_limb_t __x;                                  \
    __x = *__src++;                                 \
    if (__n != 0)                                   \
      {                                             \
        do                                          \
          {                                         \
        *__dst++ = __x;                         \
        __x = *__src++;                         \
          }                                         \
        while (--__n);                              \
      }                                             \
    *__dst++ = __x;                                 \
      }                                                 \
  } while (0)
#endif

#if ! defined (MPN_COPY_DECR) && HAVE_NATIVE_mpn_copyd
#define MPN_COPY_DECR(dst, src, size)                   \
  do {                                                  \
    ASSERT ((size) >= 0);                               \
    ASSERT (MPN_SAME_OR_DECR_P (dst, src, size));       \
    mpn_copyd (dst, src, size);                         \
  } while (0)
#endif

/* Copy N limbs from SRC to DST decrementing, N==0 allowed.  */
#if ! defined (MPN_COPY_DECR)
#define MPN_COPY_DECR(dst, src, n)                      \
  do {                                                  \
    ASSERT ((n) >= 0);                                  \
    ASSERT (MPN_SAME_OR_DECR_P (dst, src, n));          \
    if ((n) != 0)                                       \
      {                                                 \
    mp_size_t __n = (n) - 1;                        \
    mp_ptr __dst = (dst) + __n;                     \
    mp_srcptr __src = (src) + __n;                  \
    mp_limb_t __x;                                  \
    __x = *__src--;                                 \
    if (__n != 0)                                   \
      {                                             \
        do                                          \
          {                                         \
        *__dst-- = __x;                         \
        __x = *__src--;                         \
          }                                         \
        while (--__n);                              \
      }                                             \
    *__dst-- = __x;                                 \
      }                                                 \
  } while (0)
#endif


#ifndef MPN_COPY
#define MPN_COPY(d,s,n)                         \
  do {                                          \
    MPN_COPY_INCR (d, s, n);                    \
  } while (0)
#endif

typedef mp_limb_t UWtype;
typedef unsigned int UHWtype;
#define BITS_PER_MP_LIMB  (8 * sizeof(mp_limb_t))
#define W_TYPE_SIZE BITS_PER_MP_LIMB

#define __BITS4 (W_TYPE_SIZE / 4)
#define __ll_B ((UWtype) 1 << (W_TYPE_SIZE / 2))
#define __ll_lowpart(t) ((UWtype) (t) & (__ll_B - 1))
#define __ll_highpart(t) ((UWtype) (t) >> (W_TYPE_SIZE / 2))

typedef unsigned int UDItype	__attribute__ ((mode (DI)));

#define umul_ppmm(w1, w0, u, v) \
  __asm__ ("mulq %3"							\
	   : "=a" (w0), "=d" (w1)					\
	   : "%0" ((UDItype)(u)), "rm" ((UDItype)(v)))
#define udiv_qrnnd(q, r, n1, n0, dx) /* d renamed to dx avoiding "=d" */\
  __asm__ ("divq %4"		     /* stringification in K&R C */	\
	   : "=a" (q), "=d" (r)						\
	   : "0" ((UDItype)(n0)), "1" ((UDItype)(n1)), "rm" ((UDItype)(dx)))

#ifndef invert_limb
#define invert_limb(invxl,xl)                   \
  do {                                          \
    mp_limb_t dummy;                            \
    udiv_qrnnd (invxl, dummy, ~(xl), ~CNST_LIMB(0), xl);  \
	(void)dummy; \
  } while (0)
#endif

#define mpir_invert_pi1(dinv, d1, d0)					\
  do {									\
    mp_limb_t _v, _p, _t1, _t0, _mask;					\
    invert_limb (_v, d1);						\
    _p = (d1) * _v;							\
    _p += (d0);								\
    if (_p < (d0))							\
      {									\
	_v--;								\
	_mask = -(mp_limb_t) (_p >= (d1));				\
	_p -= (d1);							\
	_v += _mask;							\
	_p -= _mask & (d1);						\
      }									\
    umul_ppmm (_t1, _t0, d0, _v);					\
    _p += _t1;								\
    if (_p < _t1)							\
      {									\
	_v--;								\
	if (UNLIKELY (_p >= (d1)))					\
	  {								\
	    if (_p > (d1) || _t0 >= (d0))				\
	      _v--;							\
	  }								\
      }									\
    dinv = _v;							\
  } while (0)

#if !defined (umul_ppmm)
#define umul_ppmm(w1, w0, u, v)						\
  do {									\
    UWtype __x0, __x1, __x2, __x3;					\
    UHWtype __ul, __vl, __uh, __vh;					\
    UWtype __u = (u), __v = (v);					\
									\
    __ul = __ll_lowpart (__u);						\
    __uh = (UHWtype)__ll_highpart (__u);						\
    __vl = __ll_lowpart (__v);						\
    __vh = (UHWtype)__ll_highpart (__v);						\
									\
    __x0 = (UWtype) __ul * __vl;					\
    __x1 = (UWtype) __ul * __vh;					\
    __x2 = (UWtype) __uh * __vl;					\
    __x3 = (UWtype) __uh * __vh;					\
									\
    __x1 += __ll_highpart (__x0);/* this can't give carry */		\
    __x1 += __x2;		/* but this indeed can */		\
    if (__x1 < __x2)		/* did we get it? */			\
      __x3 += __ll_B;		/* yes, add it in the proper pos. */	\
									\
    (w1) = __x3 + __ll_highpart (__x1);					\
    (w0) = (__x1 << W_TYPE_SIZE/2) + __ll_lowpart (__x0);		\
  } while (0)
#endif

#define mpn_decr_u(p,incr)                              \
  do {                                                  \
    mp_limb_t __x;                                      \
    mp_ptr __p = (p);                                   \
    if (__builtin_constant_p (incr) && (incr) == 1)     \
      {                                                 \
        while ((*(__p++))-- == 0)                       \
          ;                                             \
      }                                                 \
    else                                                \
      {                                                 \
        __x = *__p;                                     \
        *__p = __x - (incr);                            \
        if (__x < (incr))                               \
          while ((*(++__p))-- == 0)                     \
            ;                                           \
      }                                                 \
  } while (0)


void
tdiv_qr (mp_ptr qp, mp_ptr rp,
			 mp_srcptr np, mp_size_t nn, mp_srcptr dp, mp_size_t dn)
{
//	ASSERT_ALWAYS (qxn == 0);
//
//	ASSERT (nn >= 0);
//	ASSERT (dn >= 0);
//	ASSERT (dn == 0 || dp[dn - 1] != 0);
//	ASSERT (! MPN_OVERLAP_P (qp, nn - dn + 1 + qxn, np, nn));
//	ASSERT (! MPN_OVERLAP_P (qp, nn - dn + 1 + qxn, dp, dn));

	switch (dn)
	{
	case 0:
//		DIVIDE_BY_ZERO;

	case 1:
	{
		rp[0] = mpn_divrem_1 (qp, (mp_size_t) 0, np, nn, dp[0]);
		return;
	}

	case 2:
	{
		mp_ptr n2p, d2p;
		mp_limb_t qhl, cy;
		if ((dp[1] & GMP_NUMB_HIGHBIT) == 0)
		{
			int cnt;
			mp_limb_t dtmp[2];
			count_leading_zeros (cnt, dp[1]);
			cnt -= GMP_NAIL_BITS;
			d2p = dtmp;
			d2p[1] = (dp[1] << cnt) | (dp[0] >> (GMP_NUMB_BITS - cnt));
			d2p[0] = (dp[0] << cnt) & GMP_NUMB_MASK;
			n2p = TMP_ALLOC_LIMBS (nn + 1);
			cy = mpn_lshift (n2p, np, nn, cnt);
			n2p[nn] = cy;
			qhl = mpn_divrem_2 (qp, 0L, n2p, nn + (cy != 0), d2p);
			if (cy == 0)
				qp[nn - 2] = qhl;	/* always store nn-2+1 quotient limbs */
			rp[0] = (n2p[0] >> cnt)
					| ((n2p[1] << (GMP_NUMB_BITS - cnt)) & GMP_NUMB_MASK);
			rp[1] = (n2p[1] >> cnt);
		}
		else
		{
			d2p = (mp_ptr) dp;
			n2p = TMP_ALLOC_LIMBS (nn);
			MPN_COPY (n2p, np, nn);
			qhl = mpn_divrem_2 (qp, 0L, n2p, nn, d2p);
			qp[nn - 2] = qhl;	/* always store nn-2+1 quotient limbs */
			rp[0] = n2p[0];
			rp[1] = n2p[1];
		}
		return;
	}

	default:
	{
		int adjust;
		mp_limb_t dinv;
		adjust = np[nn - 1] >= dp[dn - 1];	/* conservative tests for quotient size */
		if (nn + adjust >= 2 * dn)
		{
			mp_ptr n2p, d2p;
			mp_limb_t cy;
			int cnt;

			qp[nn - dn] = 0;			  /* zero high quotient limb */
			if ((dp[dn - 1] & GMP_NUMB_HIGHBIT) == 0) /* normalize divisor */
			{
				count_leading_zeros (cnt, dp[dn - 1]);
				cnt -= GMP_NAIL_BITS;
				d2p = TMP_ALLOC_LIMBS (dn);
				mpn_lshift (d2p, dp, dn, cnt);
				n2p = TMP_ALLOC_LIMBS (nn + 1);
				cy = mpn_lshift (n2p, np, nn, cnt);
				n2p[nn] = cy;
				nn += adjust;
			}
			else
			{
				cnt = 0;
				d2p = (mp_ptr) dp;
				n2p = TMP_ALLOC_LIMBS (nn + 1);
				MPN_COPY (n2p, np, nn);
				n2p[nn] = 0;
				nn += adjust;
			}

			mpir_invert_pi1 (dinv, d2p[dn - 1], d2p[dn - 2]);
			mpn_sb_div_qr (qp, n2p, nn, d2p, dn, dinv);

			if (cnt != 0)
				mpn_rshift (rp, n2p, dn, cnt);
			else
				MPN_COPY (rp, n2p, dn);
			return;
		}

		/* When we come here, the numerator/partial remainder is less
		   than twice the size of the denominator.  */

		{
			/* Problem:

			   Divide a numerator N with nn limbs by a denominator D with dn
			   limbs forming a quotient of qn=nn-dn+1 limbs.  When qn is small
			   compared to dn, conventional division algorithms perform poorly.
			   We want an algorithm that has an expected running time that is
			   dependent only on qn.

			   Algorithm (very informally stated):

			   1) Divide the 2 x qn most significant limbs from the numerator
			  by the qn most significant limbs from the denominator.  Call
			  the result qest.  This is either the correct quotient, but
			  might be 1 or 2 too large.  Compute the remainder from the
			  division.  (This step is implemented by a mpn_divrem call.)

			   2) Is the most significant limb from the remainder < p, where p
			  is the product of the most significant limb from the quotient
			  and the next(d)?  (Next(d) denotes the next ignored limb from
			  the denominator.)  If it is, decrement qest, and adjust the
			  remainder accordingly.

			   3) Is the remainder >= qest?  If it is, qest is the desired
			  quotient.  The algorithm terminates.

			   4) Subtract qest x next(d) from the remainder.  If there is
			  borrow out, decrement qest, and adjust the remainder
			  accordingly.

			   5) Skip one word from the denominator (i.e., let next(d) denote
			  the next less significant limb.  */

			mp_size_t qn;
			mp_ptr n2p, d2p;
			mp_ptr tp;
			mp_limb_t cy;
			mp_size_t in, rn;
			mp_limb_t quotient_too_large;
			unsigned int cnt;

			qn = nn - dn;
			qp[qn] = 0;				/* zero high quotient limb */
			qn += adjust;			/* qn cannot become bigger */

			if (qn == 0)
			{
				MPN_COPY (rp, np, dn);
				TMP_FREE;
				return;
			}

			in = dn - qn;		/* (at least partially) ignored # of limbs in ops */
			/* Normalize denominator by shifting it to the left such that its
			   most significant bit is set.  Then shift the numerator the same
			   amount, to mathematically preserve quotient.  */
			if ((dp[dn - 1] & GMP_NUMB_HIGHBIT) == 0)
			{
				count_leading_zeros (cnt, dp[dn - 1]);
				cnt -= GMP_NAIL_BITS;

				d2p = TMP_ALLOC_LIMBS (qn);
				mpn_lshift (d2p, dp + in, qn, cnt);
				d2p[0] |= dp[in - 1] >> (GMP_NUMB_BITS - cnt);

				n2p = TMP_ALLOC_LIMBS (2 * qn + 1);
				cy = mpn_lshift (n2p, np + nn - 2 * qn, 2 * qn, cnt);
				if (adjust)
				{
					n2p[2 * qn] = cy;
					n2p++;
				}
				else
				{
					n2p[0] |= np[nn - 2 * qn - 1] >> (GMP_NUMB_BITS - cnt);
				}
			}
			else
			{
				cnt = 0;
				d2p = (mp_ptr) dp + in;

				n2p = TMP_ALLOC_LIMBS (2 * qn + 1);
				MPN_COPY (n2p, np + nn - 2 * qn, 2 * qn);
				if (adjust)
				{
					n2p[2 * qn] = 0;
					n2p++;
				}
			}

			/* Get an approximate quotient using the extracted operands.  */
			if (qn == 1)
			{
				mp_limb_t q0, r0;
				udiv_qrnnd (q0, r0, n2p[1], n2p[0] << GMP_NAIL_BITS, d2p[0] << GMP_NAIL_BITS);
				n2p[0] = r0 >> GMP_NAIL_BITS;
				qp[0] = q0;
			}
			else if (qn == 2)
				mpn_divrem_2 (qp, 0L, n2p, 4L, d2p); /* FIXME: obsolete function */
			else
			{
				mpir_invert_pi1 (dinv, d2p[qn - 1], d2p[qn - 2]);
				mpn_sb_div_qr (qp, n2p, 2 * qn, d2p, qn, dinv);
			}

			rn = qn;
			/* Multiply the first ignored divisor limb by the most significant
			   quotient limb.  If that product is > the partial remainder's
			   most significant limb, we know the quotient is too large.  This
			   test quickly catches most cases where the quotient is too large;
			   it catches all cases where the quotient is 2 too large.  */
			{
				mp_limb_t dl, x;
				mp_limb_t h, dummy;

				if (in - 2 < 0)
					dl = 0;
				else
					dl = dp[in - 2];

#if GMP_NAIL_BITS == 0
				x = (dp[in - 1] << cnt) | ((dl >> 1) >> ((~cnt) % GMP_LIMB_BITS));
#else
				x = (dp[in - 1] << cnt) & GMP_NUMB_MASK;
	      if (cnt != 0)
		x |= dl >> (GMP_NUMB_BITS - cnt);
#endif
				umul_ppmm (h, dummy, x, qp[qn - 1] << GMP_NAIL_BITS);
				(void) dummy;

				if (n2p[qn - 1] < h)
				{
					mp_limb_t cy;

					mpn_decr_u (qp, (mp_limb_t) 1);
					cy = mpn_add_n (n2p, n2p, d2p, qn);
					if (cy)
					{
						/* The partial remainder is safely large.  */
						n2p[qn] = cy;
						++rn;
					}
				}
			}

			quotient_too_large = 0;
			if (cnt != 0)
			{
				mp_limb_t cy1, cy2;

				/* Append partially used numerator limb to partial remainder.  */
				cy1 = mpn_lshift (n2p, n2p, rn, GMP_NUMB_BITS - cnt);
				n2p[0] |= np[in - 1] & (GMP_NUMB_MASK >> cnt);

				/* Update partial remainder with partially used divisor limb.  */
				cy2 = mpn_submul_1 (n2p, qp, qn, dp[in - 1] & (GMP_NUMB_MASK >> cnt));
				if (qn != rn)
				{
//					ASSERT_ALWAYS (n2p[qn] >= cy2);
					n2p[qn] -= cy2;
				}
				else
				{
					n2p[qn] = cy1 - cy2; /* & GMP_NUMB_MASK; */

					quotient_too_large = (cy1 < cy2);
					++rn;
				}
				--in;
			}
			/* True: partial remainder now is neutral, i.e., it is not shifted up.  */

			tp = TMP_ALLOC_LIMBS (dn);

			if (in < qn)
			{
				if (in == 0)
				{
					MPN_COPY (rp, n2p, rn);
//					ASSERT_ALWAYS (rn == dn);
					goto foo;
				}
				mpn_mul (tp, qp, qn, dp, in);
			}
			else
				mpn_mul (tp, dp, in, qp, qn);

			cy = mpn_sub (n2p, n2p, rn, tp + in, qn);
			MPN_COPY (rp + in, n2p, dn - in);
			quotient_too_large |= cy;
			cy = mpn_sub_n (rp, np, tp, in);
			cy = mpn_sub_1 (rp + in, rp + in, rn, cy);
			quotient_too_large |= cy;
			foo:
			if (quotient_too_large)
			{
				mpn_decr_u (qp, (mp_limb_t) 1);
				mpn_add_n (rp, rp, dp, dn);
			}
		}
		TMP_FREE;
		return;
	}
	}
}



static_assert(sizeof(evm_uint256be) == 32, "evm_uint256be is too big");
static_assert(sizeof(evm_address) == 20, "evm_address is too big");
static_assert(sizeof(evm_result) == 64, "evm_result does not fit cache line");
static_assert(sizeof(evm_message) <= 18*8, "evm_message not optimally packed");
static_assert(offsetof(evm_message, code_hash) % 8 == 0, "evm_message.code_hash not aligned");

// Check enums match int size.
// On GCC/clang the underlying type should be unsigned int, on MSVC int
static_assert(sizeof(evm_call_kind)  == sizeof(int), "Enum `evm_call_kind` is not the size of int");
static_assert(sizeof(evm_revision)       == sizeof(int), "Enum `evm_revision` is not the size of int");


namespace dev
{
namespace evmjit
{
using namespace eth::jit;

namespace
{
using ExecFunc = ReturnCode(*)(ExecutionContext*);

char toChar(evm_revision rev)
{
	switch (rev)
	{
	case EVM_FRONTIER: return 'F';
	case EVM_HOMESTEAD: return 'H';
	case EVM_TANGERINE_WHISTLE: return 'T';
	case EVM_SPURIOUS_DRAGON: return 'S';
	case EVM_BYZANTIUM: return 'B';
	case EVM_CONSTANTINOPLE: return 'C';
	}
	LLVM_BUILTIN_UNREACHABLE;
}

/// Combine code hash and EVM revision into a printable code identifier.
std::string makeCodeId(evm_uint256be codeHash, evm_revision rev, uint32_t flags)
{
	static const auto hexChars = "0123456789abcdef";
	std::string str;
	str.reserve(sizeof(codeHash) * 2 + 1);
	for (auto b: codeHash.bytes)
	{
		str.push_back(hexChars[b & 0xf]);
		str.push_back(hexChars[b >> 4]);
	}
	str.push_back(toChar(rev));
	if (flags & EVM_STATIC)
		str.push_back('S');
	return str;
}

void printVersion()
{
	std::cout << "Ethereum EVM JIT Compiler (http://github.com/ethereum/evmjit):\n"
			  << "  EVMJIT version " << EVMJIT_VERSION << "\n"
#ifdef NDEBUG
			  << "  Optimized build, "
#else
			  << "  DEBUG build, "
#endif
			  << __DATE__ << " (" << __TIME__ << ")\n"
			  << std::endl;
}

namespace cl = llvm::cl;
cl::opt<bool> g_optimize{"O", cl::desc{"Optimize"}};
cl::opt<CacheMode> g_cache{"cache", cl::desc{"Cache compiled EVM code on disk"},
	cl::values(
		clEnumValN(CacheMode::off,   "0", "Disabled"),
		clEnumValN(CacheMode::on,    "1", "Enabled"),
		clEnumValN(CacheMode::read,  "r", "Read only. No new objects are added to cache."),
		clEnumValN(CacheMode::write, "w", "Write only. No objects are loaded from cache."),
		clEnumValN(CacheMode::clear, "c", "Clear the cache storage. Cache is disabled."),
		clEnumValN(CacheMode::preload, "p", "Preload all cached objects."),
		clEnumValEnd)};
cl::opt<bool> g_stats{"st", cl::desc{"Statistics"}};
cl::opt<bool> g_dump{"dump", cl::desc{"Dump LLVM IR module"}};

void parseOptions()
{
	static llvm::llvm_shutdown_obj shutdownObj{};
	cl::AddExtraVersionPrinter(printVersion);
	cl::ParseEnvironmentOptions("evmjit", "EVMJIT", "Ethereum EVM JIT Compiler");
}

class JITImpl: public evm_instance
{
	std::unique_ptr<llvm::ExecutionEngine> m_engine;
	mutable std::mutex x_codeMap;
	std::unordered_map<std::string, ExecFunc> m_codeMap;

	static llvm::LLVMContext& getLLVMContext()
	{
		// TODO: This probably should be thread_local, but for now that causes
		// a crash when MCJIT is destroyed.
		static llvm::LLVMContext llvmContext;
		return llvmContext;
	}

public:
	static JITImpl& instance()
	{
		// We need to keep this a singleton.
		// so we only call changeVersion on it.
		static JITImpl s_instance;
		return s_instance;
	}

	JITImpl();

	llvm::ExecutionEngine& engine() { return *m_engine; }

	ExecFunc getExecFunc(std::string const& _codeIdentifier) const;
	void mapExecFunc(std::string const& _codeIdentifier, ExecFunc _funcAddr);

	ExecFunc compile(evm_revision _rev, bool _staticCall, byte const* _code, uint64_t _codeSize, std::string const& _codeIdentifier);

	evm_context_fn_table const* host = nullptr;

	evm_message const* currentMsg = nullptr;
	std::vector<uint8_t> returnBuffer;
};

int64_t call_v2(
	evm_context* _ctx,
	int _kind,
	int64_t _gas,
	evm_address const* _address,
	evm_uint256be const* _value,
	uint8_t const* _inputData,
	size_t _inputSize,
	uint8_t* _outputData,
	size_t _outputSize,
	uint8_t const** o_bufData,
	size_t* o_bufSize
) noexcept
{
	auto& jit = JITImpl::instance();

	evm_message msg;
	msg.address = *_address;
	msg.sender = _kind != EVM_DELEGATECALL ? jit.currentMsg->address : jit.currentMsg->sender;
	msg.value = _kind != EVM_DELEGATECALL ? *_value : jit.currentMsg->value;
	msg.input = _inputData;
	msg.input_size = _inputSize;
	msg.gas = _gas;
	msg.depth = jit.currentMsg->depth + 1;
	msg.flags = jit.currentMsg->flags;
	if (_kind == EVM_STATICCALL)
	{
		msg.kind = EVM_CALL;
		msg.flags |= EVM_STATIC;
	}
	else
		msg.kind = static_cast<evm_call_kind>(_kind);

	// FIXME: Handle code hash.
	evm_result result;
	jit.host->call(&result, _ctx, &msg);
	// FIXME: Clarify when gas_left is valid.
	int64_t r = result.gas_left;

	// Handle output. It can contain data from RETURN or REVERT opcodes.
	auto size = std::min(_outputSize, result.output_size);
	std::copy(result.output_data, result.output_data + size, _outputData);
	jit.returnBuffer = {result.output_data, result.output_data + result.output_size};

	*o_bufData = jit.returnBuffer.data();
	*o_bufSize = jit.returnBuffer.size();

	if (result.status_code != EVM_SUCCESS)
		r |= EVM_CALL_FAILURE;

	if (result.release)
		result.release(&result);
	return r;
}

template<size_t N>
void div(uint64_t q[], uint64_t r[], const uint64_t n[], const uint64_t d[])
{
	// We don't have to zero everything, but only top limbs that MPIR will
	// not touch. But simple constant-length memset might be actually faster.
	std::fill_n(q, N, 0);
	std::fill_n(r, N, 0);

	size_t d_size = N;
	for (size_t i = 0; i < N; ++i)
	{
		if (d[N - 1 - i] != 0)
			break;
		--d_size;
	}

	if (d_size == 0)
		return;

	tdiv_qr(q, r, n, N, d, d_size);

//	size_t q_size = N - d_size + 1;
//	gmp_printf("DIV %Nx / %Nx = %Nx, %Nx\n", n, N, d, d_size, q, q_size, r, d_size);
}


class SymbolResolver : public llvm::SectionMemoryManager
{
	llvm::RuntimeDyld::SymbolInfo findSymbol(std::string const& _name) override
	{
		auto& jit = JITImpl::instance();

		// Handle symbols' global prefix.
		// If in current DataLayout global symbols are prefixed, drop the
		// prefix from the name for local search.
		char prefix = jit.engine().getDataLayout().getGlobalPrefix();
		llvm::StringRef unprefixedName = (prefix != '\0' && _name[0] == prefix)
			? llvm::StringRef{_name}.drop_front() : llvm::StringRef{_name};

		auto addr = llvm::StringSwitch<uint64_t>(unprefixedName)
			.Case("env_sha3", reinterpret_cast<uint64_t>(&keccak))
			.Case("evm.exists", reinterpret_cast<uint64_t>(jit.host->account_exists))
			.Case("evm.sload", reinterpret_cast<uint64_t>(jit.host->get_storage))
			.Case("evm.sstore", reinterpret_cast<uint64_t>(jit.host->set_storage))
			.Case("evm.balance", reinterpret_cast<uint64_t>(jit.host->get_balance))
			.Case("evm.code", reinterpret_cast<uint64_t>(jit.host->get_code))
			.Case("evm.selfdestruct", reinterpret_cast<uint64_t>(jit.host->selfdestruct))
			.Case("evm.call", reinterpret_cast<uint64_t>(call_v2))
			.Case("evm.get_tx_context", reinterpret_cast<uint64_t>(jit.host->get_tx_context))
			.Case("evm.blockhash", reinterpret_cast<uint64_t>(jit.host->get_block_hash))
			.Case("evm.log", reinterpret_cast<uint64_t>(jit.host->log))
			.Case("external.evm.udivrem.i256", reinterpret_cast<uint64_t>(div<4>))
			.Case("external.evm.udivrem.i512", reinterpret_cast<uint64_t>(div<8>))
			.Default(0);
		if (addr)
			return {addr, llvm::JITSymbolFlags::Exported};

		// Fallback to default implementation that would search for the symbol
		// in the current process. Use the original prefixed symbol name.
		// TODO: In the future we should control the whole set of requested
		//       symbols (like memcpy, memset, etc) to improve performance.
		return llvm::SectionMemoryManager::findSymbol(_name);
	}

	void reportMemorySize(size_t _addedSize)
	{
		if (!g_stats)
			return;

		m_totalMemorySize += _addedSize;
		if (m_totalMemorySize >= m_printMemoryLimit)
		{
			static const auto M = 1024 * 1024;
			auto value = double(m_totalMemorySize) / M;
			std::cerr << "EVMJIT total memory size: " << value << '\n';
			m_printMemoryLimit += M;
		}
	}

	uint8_t* allocateCodeSection(uintptr_t _size, unsigned _a, unsigned _id,
	                             llvm::StringRef _name) override
	{
		reportMemorySize(_size);
		return llvm::SectionMemoryManager::allocateCodeSection(_size, _a, _id, _name);
	}

	uint8_t* allocateDataSection(uintptr_t _size, unsigned _a, unsigned _id,
	                             llvm::StringRef _name, bool _ro) override
	{
		reportMemorySize(_size);
		return llvm::SectionMemoryManager::allocateDataSection(_size, _a, _id, _name, _ro);
	}

	size_t m_totalMemorySize = 0;
	size_t m_printMemoryLimit = 1024 * 1024;
};




ExecFunc JITImpl::getExecFunc(std::string const& _codeIdentifier) const
{
	std::lock_guard<std::mutex> lock{x_codeMap};
	auto it = m_codeMap.find(_codeIdentifier);
	if (it != m_codeMap.end())
		return it->second;
	return nullptr;
}

void JITImpl::mapExecFunc(std::string const& _codeIdentifier, ExecFunc _funcAddr)
{
	std::lock_guard<std::mutex> lock{x_codeMap};
	m_codeMap.emplace(_codeIdentifier, _funcAddr);
}

ExecFunc JITImpl::compile(evm_revision _rev, bool _staticCall, byte const* _code, uint64_t _codeSize,
	std::string const& _codeIdentifier)
{
	auto module = Cache::getObject(_codeIdentifier, getLLVMContext());
	if (!module)
	{
		// TODO: Listener support must be redesigned. These should be a feature of JITImpl
		//listener->stateChanged(ExecState::Compilation);
		assert(_code || !_codeSize);
		//TODO: Can the Compiler be stateless?
		module = Compiler({}, _rev, _staticCall, getLLVMContext()).compile(_code, _code + _codeSize, _codeIdentifier);

		if (g_optimize)
		{
			//listener->stateChanged(ExecState::Optimization);
			optimize(*module);
		}

		prepare(*module);
	}
	if (g_dump)
		module->dump();

	m_engine->addModule(std::move(module));
	//listener->stateChanged(ExecState::CodeGen);
	return (ExecFunc)m_engine->getFunctionAddress(_codeIdentifier);
}

} // anonymous namespace


ExecutionContext::~ExecutionContext() noexcept
{
	if (m_memData)
		std::free(m_memData);
}

bytes_ref ExecutionContext::getReturnData() const
{
	auto data = m_data->callData;
	auto size = static_cast<size_t>(m_data->callDataSize);

	if (data < m_memData || data >= m_memData + m_memSize || size == 0)
	{
		assert(size == 0); // data can be an invalid pointer only if size is 0
		m_data->callData = nullptr;
		return {};
	}

	return bytes_ref{data, size};
}


extern "C"
{

EXPORT evm_instance* evmjit_create()
{
	// Let's always return the same instance. It's a bit of faking, but actually
	// this might be a compliant implementation.
	return &JITImpl::instance();
}

static void destroy(evm_instance* instance)
{
	(void)instance;
	assert(instance == static_cast<void*>(&JITImpl::instance()));
}

static evm_result execute(evm_instance* instance, evm_context* context, evm_revision rev,
	evm_message const* msg, uint8_t const* code, size_t code_size)
{
	auto& jit = *reinterpret_cast<JITImpl*>(instance);
	if (!jit.host)
		jit.host = context->fn_table;
	assert(jit.host == context->fn_table);  // Require the fn_table not to change.

	// TODO: Temporary keep track of the current message.
	evm_message const* prevMsg = jit.currentMsg;
	jit.currentMsg = msg;

	RuntimeData rt;
	rt.code = code;
	rt.codeSize = code_size;
	rt.gas = msg->gas;
	rt.callData = msg->input;
	rt.callDataSize = msg->input_size;
	std::memcpy(&rt.apparentValue, &msg->value, sizeof(msg->value));
	std::memset(&rt.address, 0, 12);
	std::memcpy(&rt.address[12], &msg->address, sizeof(msg->address));
	std::memset(&rt.caller, 0, 12);
	std::memcpy(&rt.caller[12], &msg->sender, sizeof(msg->sender));
	rt.depth = msg->depth;

	ExecutionContext ctx{rt, context};

	evm_result result;
	result.status_code = EVM_SUCCESS;
	result.gas_left = 0;
	result.output_data = nullptr;
	result.output_size = 0;
	result.release = nullptr;

	auto codeIdentifier = makeCodeId(msg->code_hash, rev, msg->flags);
	auto execFunc = jit.getExecFunc(codeIdentifier);
	if (!execFunc)
	{
		bool const staticCall = (msg->flags & EVM_STATIC) != 0;
		execFunc = jit.compile(rev, staticCall, ctx.code(), ctx.codeSize(), codeIdentifier);
		if (!execFunc)
			return result;
		jit.mapExecFunc(codeIdentifier, execFunc);
	}

	auto returnCode = execFunc(&ctx);

	if (returnCode == ReturnCode::Revert)
	{
		result.status_code = EVM_REVERT;
		result.gas_left = rt.gas;
	}
	else if (returnCode == ReturnCode::OutOfGas)
	{
		// EVMJIT does not provide information what exactly type of failure
		// it was, so use generic EVM_FAILURE.
		result.status_code = EVM_FAILURE;
	}
	else
	{
		// In case of success return the amount of gas left.
		result.gas_left = rt.gas;
	}

	if (returnCode == ReturnCode::Return || returnCode == ReturnCode::Revert)
	{
		auto out = ctx.getReturnData();
		result.output_data = std::get<0>(out);
		result.output_size = std::get<1>(out);
	}

	// Take care of the internal memory.
	if (ctx.m_memData)
	{
		// Use result's reserved data to store the memory pointer.
		result.reserved.context = ctx.m_memData;

		// Set pointer to the destructor that will release the memory.
		result.release = [](evm_result const* result)
		{
			std::free(result->reserved.context);
		};
		ctx.m_memData = nullptr;
	}

	jit.currentMsg = prevMsg;
	return result;
}

static int set_option(evm_instance* instance, char const* name,
	char const* value)
{
	(void)instance, (void)name, (void)value;
	return 0;
}

static evm_code_status get_code_status(evm_instance* instance,
	evm_revision rev, uint32_t flags, evm_uint256be code_hash)
{
	auto& jit = *reinterpret_cast<JITImpl*>(instance);
	auto codeIdentifier = makeCodeId(code_hash, rev, flags);
	if (jit.getExecFunc(codeIdentifier) != nullptr)
		return EVM_READY;
	// TODO: Add support for EVM_CACHED.
	return EVM_UNKNOWN;
}

static void prepare_code(evm_instance* instance, evm_revision rev, uint32_t flags,
	evm_uint256be code_hash, unsigned char const* code, size_t code_size)
{
	auto& jit = *reinterpret_cast<JITImpl*>(instance);
	auto codeIdentifier = makeCodeId(code_hash, rev, flags);
	bool const staticCall = (flags & EVM_STATIC) != 0;
	auto execFunc = jit.compile(rev, staticCall, code, code_size, codeIdentifier);
	if (execFunc) // FIXME: What with error?
		jit.mapExecFunc(codeIdentifier, execFunc);
}

}  // extern "C"

JITImpl::JITImpl():
		evm_instance({EVM_ABI_VERSION,
		              evmjit::destroy,
		              evmjit::execute,
		              evmjit::get_code_status,
		              evmjit::prepare_code,
		              evmjit::set_option})
{
	parseOptions();

	bool preloadCache = g_cache == CacheMode::preload;
	if (preloadCache)
		g_cache = CacheMode::on;

	llvm::InitializeNativeTarget();
	llvm::InitializeNativeTargetAsmPrinter();

	auto module = llvm::make_unique<llvm::Module>("", getLLVMContext());

	// FIXME: LLVM 3.7: test on Windows
	auto triple = llvm::Triple(llvm::sys::getProcessTriple());
	if (triple.getOS() == llvm::Triple::OSType::Win32)
		triple.setObjectFormat(llvm::Triple::ObjectFormatType::ELF);  // MCJIT does not support COFF format
	module->setTargetTriple(triple.str());

	llvm::EngineBuilder builder(std::move(module));
	builder.setEngineKind(llvm::EngineKind::JIT);
	builder.setMCJITMemoryManager(llvm::make_unique<SymbolResolver>());
	builder.setOptLevel(g_optimize ? llvm::CodeGenOpt::Default : llvm::CodeGenOpt::None);
#ifndef NDEBUG
	builder.setVerifyModules(true);
#endif

	m_engine.reset(builder.create());

	// TODO: Update cache listener
	m_engine->setObjectCache(Cache::init(g_cache, nullptr));

	// FIXME: Disabled during API changes
	//if (preloadCache)
	//	Cache::preload(*m_engine, funcCache);
}

}
}
