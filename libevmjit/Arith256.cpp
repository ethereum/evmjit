#include "Arith256.h"

#include <iostream>
#include <iomanip>

#include "preprocessor/llvm_includes_start.h"
#include <llvm/IR/Module.h>
#include <llvm/IR/IntrinsicInst.h>
#include "preprocessor/llvm_includes_end.h"

#include "Type.h"
#include "Endianness.h"
#include "Utils.h"

namespace dev
{
namespace eth
{
namespace jit
{

Arith256::Arith256(IRBuilder& _builder) :
	CompilerHelper(_builder)
{}

void Arith256::debug(llvm::Value* _value, char _c, llvm::Module& _module, IRBuilder& _builder)
{
	static const auto funcName = "debug";
	auto func = _module.getFunction(funcName);
	if (!func)
		func = llvm::Function::Create(llvm::FunctionType::get(Type::Void, {Type::Word, _builder.getInt8Ty()}, false), llvm::Function::ExternalLinkage, funcName, &_module);

	_builder.CreateCall(func, {_builder.CreateZExtOrTrunc(_value, Type::Word), _builder.getInt8(_c)});
}

namespace
{
llvm::Function* createUDivRemFunc(llvm::Type* _type, llvm::Module& _module, char const* _funcName)
{
	// Based of "Improved shift divisor algorithm" from "Software Integer Division" by Microsoft Research
	// The following algorithm also handles divisor of value 0 returning 0 for both quotient and remainder

	auto retType = llvm::VectorType::get(_type, 2);
	auto func = llvm::Function::Create(llvm::FunctionType::get(retType, {_type, _type}, false), llvm::Function::PrivateLinkage, _funcName, &_module);
	func->setDoesNotThrow();
	func->setDoesNotAccessMemory();

	auto iter = func->arg_begin();
	llvm::Argument* x = &(*iter++);
	x->setName("x");
	llvm::Argument* y = &(*iter);
	y->setName("y");

	auto bb = llvm::BasicBlock::Create(_module.getContext(), {}, func);
	auto builder = IRBuilder{bb};
	auto allocaQ = builder.CreateAlloca(_type, nullptr, "alloca.q");
	auto allocaR = builder.CreateAlloca(_type, nullptr, "alloca.r");
	auto allocaN = builder.CreateAlloca(_type, nullptr, "alloca.n");
	auto allocaD = builder.CreateAlloca(_type, nullptr, "alloca.d");
	builder.CreateStore(x, allocaN);
	builder.CreateStore(y, allocaD);

	auto ptrTy = _type->getPointerTo();

	auto extFunc = llvm::Function::Create(
		llvm::FunctionType::get(builder.getVoidTy(), {ptrTy, ptrTy, ptrTy, ptrTy}, false),
		llvm::Function::ExternalLinkage,
		{"external.", llvm::StringRef{_funcName}},
		&_module
	);

	builder.CreateCall(extFunc, {allocaQ, allocaR, allocaN, allocaD});
	auto q = builder.CreateLoad(allocaQ);
	auto r = builder.CreateLoad(allocaR);
	auto ret = builder.CreateInsertElement(llvm::UndefValue::get(retType), q, uint64_t(0), "ret0");
	ret = builder.CreateInsertElement(ret, r, 1, "ret");
	builder.CreateRet(ret);

	return func;
}
}

llvm::Function* Arith256::getUDivRem256Func(llvm::Module& _module)
{
	static const auto funcName = "evm.udivrem.i256";
	if (auto func = _module.getFunction(funcName))
		return func;

	return createUDivRemFunc(Type::Word, _module, funcName);
}

llvm::Function* Arith256::getUDivRem512Func(llvm::Module& _module)
{
	static const auto funcName = "evm.udivrem.i512";
	if (auto func = _module.getFunction(funcName))
		return func;

	return createUDivRemFunc(llvm::IntegerType::get(_module.getContext(), 512), _module, funcName);
}

llvm::Function* Arith256::getUDiv256Func(llvm::Module& _module)
{
	static const auto funcName = "evm.udiv.i256";
	if (auto func = _module.getFunction(funcName))
		return func;

	auto udivremFunc = getUDivRem256Func(_module);

	auto func = llvm::Function::Create(llvm::FunctionType::get(Type::Word, {Type::Word, Type::Word}, false), llvm::Function::PrivateLinkage, funcName, &_module);
	func->setDoesNotThrow();
	func->setDoesNotAccessMemory();

	auto iter = func->arg_begin();
	llvm::Argument* x = &(*iter++);
	x->setName("x");
	llvm::Argument* y = &(*iter);
	y->setName("y");

	auto bb = llvm::BasicBlock::Create(_module.getContext(), {}, func);
	auto builder = IRBuilder{bb};
	auto udivrem = builder.CreateCall(udivremFunc, {x, y});
	auto udiv = builder.CreateExtractElement(udivrem, uint64_t(0));
	builder.CreateRet(udiv);

	return func;
}

namespace
{
llvm::Function* createURemFunc(llvm::Type* _type, llvm::Module& _module, char const* _funcName)
{
	auto udivremFunc = _type == Type::Word ? Arith256::getUDivRem256Func(_module) : Arith256::getUDivRem512Func(_module);

	auto func = llvm::Function::Create(llvm::FunctionType::get(_type, {_type, _type}, false), llvm::Function::PrivateLinkage, _funcName, &_module);
	func->setDoesNotThrow();
	func->setDoesNotAccessMemory();

	auto iter = func->arg_begin();
	llvm::Argument* x = &(*iter++);
	x->setName("x");
	llvm::Argument* y = &(*iter);
	y->setName("y");

	auto bb = llvm::BasicBlock::Create(_module.getContext(), {}, func);
	auto builder = IRBuilder{bb};
	auto udivrem = builder.CreateCall(udivremFunc, {x, y});
	auto r = builder.CreateExtractElement(udivrem, uint64_t(1));
	builder.CreateRet(r);

	return func;
}
}

llvm::Function* Arith256::getURem256Func(llvm::Module& _module)
{
	static const auto funcName = "evm.urem.i256";
	if (auto func = _module.getFunction(funcName))
		return func;
	return createURemFunc(Type::Word, _module, funcName);
}

llvm::Function* Arith256::getURem512Func(llvm::Module& _module)
{
	static const auto funcName = "evm.urem.i512";
	if (auto func = _module.getFunction(funcName))
		return func;
	return createURemFunc(llvm::IntegerType::get(_module.getContext(), 512), _module, funcName);
}

llvm::Function* Arith256::getSDivRem256Func(llvm::Module& _module)
{
	static const auto funcName = "evm.sdivrem.i256";
	if (auto func = _module.getFunction(funcName))
		return func;

	auto udivremFunc = getUDivRem256Func(_module);

	auto retType = llvm::VectorType::get(Type::Word, 2);
	auto func = llvm::Function::Create(llvm::FunctionType::get(retType, {Type::Word, Type::Word}, false), llvm::Function::PrivateLinkage, funcName, &_module);
	func->setDoesNotThrow();
	func->setDoesNotAccessMemory();

	auto iter = func->arg_begin();
	llvm::Argument* x = &(*iter++);
	x->setName("x");
	llvm::Argument* y = &(*iter);
	y->setName("y");

	auto bb = llvm::BasicBlock::Create(_module.getContext(), "", func);
	auto builder = IRBuilder{bb};
	auto xIsNeg = builder.CreateICmpSLT(x, Constant::get(0));
	auto xNeg = builder.CreateSub(Constant::get(0), x);
	auto xAbs = builder.CreateSelect(xIsNeg, xNeg, x);

	auto yIsNeg = builder.CreateICmpSLT(y, Constant::get(0));
	auto yNeg = builder.CreateSub(Constant::get(0), y);
	auto yAbs = builder.CreateSelect(yIsNeg, yNeg, y);

	auto res = builder.CreateCall(udivremFunc, {xAbs, yAbs});
	auto qAbs = builder.CreateExtractElement(res, uint64_t(0));
	auto rAbs = builder.CreateExtractElement(res, 1);

	// the remainder has the same sign as dividend
	auto rNeg = builder.CreateSub(Constant::get(0), rAbs);
	auto r = builder.CreateSelect(xIsNeg, rNeg, rAbs);

	auto qNeg = builder.CreateSub(Constant::get(0), qAbs);
	auto xyOpposite = builder.CreateXor(xIsNeg, yIsNeg);
	auto q = builder.CreateSelect(xyOpposite, qNeg, qAbs);

	auto ret = builder.CreateInsertElement(llvm::UndefValue::get(retType), q, uint64_t(0));
	ret = builder.CreateInsertElement(ret, r, 1);
	builder.CreateRet(ret);

	return func;
}

llvm::Function* Arith256::getSDiv256Func(llvm::Module& _module)
{
	static const auto funcName = "evm.sdiv.i256";
	if (auto func = _module.getFunction(funcName))
		return func;

	auto sdivremFunc = getSDivRem256Func(_module);

	auto func = llvm::Function::Create(llvm::FunctionType::get(Type::Word, {Type::Word, Type::Word}, false), llvm::Function::PrivateLinkage, funcName, &_module);
	func->setDoesNotThrow();
	func->setDoesNotAccessMemory();

	auto iter = func->arg_begin();
	llvm::Argument* x = &(*iter++);
	x->setName("x");
	llvm::Argument* y = &(*iter);
	y->setName("y");

	auto bb = llvm::BasicBlock::Create(_module.getContext(), {}, func);
	auto builder = IRBuilder{bb};
	auto sdivrem = builder.CreateCall(sdivremFunc, {x, y});
	auto q = builder.CreateExtractElement(sdivrem, uint64_t(0));
	builder.CreateRet(q);

	return func;
}

llvm::Function* Arith256::getSRem256Func(llvm::Module& _module)
{
	static const auto funcName = "evm.srem.i256";
	if (auto func = _module.getFunction(funcName))
		return func;

	auto sdivremFunc = getSDivRem256Func(_module);

	auto func = llvm::Function::Create(llvm::FunctionType::get(Type::Word, {Type::Word, Type::Word}, false), llvm::Function::PrivateLinkage, funcName, &_module);
	func->setDoesNotThrow();
	func->setDoesNotAccessMemory();

	auto iter = func->arg_begin();
	llvm::Argument* x = &(*iter++);
	x->setName("x");
	llvm::Argument* y = &(*iter);
	y->setName("y");

	auto bb = llvm::BasicBlock::Create(_module.getContext(), {}, func);
	auto builder = IRBuilder{bb};
	auto sdivrem = builder.CreateCall(sdivremFunc, {x, y});
	auto r = builder.CreateExtractElement(sdivrem, uint64_t(1));
	builder.CreateRet(r);

	return func;
}

llvm::Function* Arith256::getExpFunc()
{
	if (!m_exp)
	{
		llvm::Type* argTypes[] = {Type::Word, Type::Word};
		m_exp = llvm::Function::Create(llvm::FunctionType::get(Type::Word, argTypes, false), llvm::Function::PrivateLinkage, "exp", getModule());
		m_exp->setDoesNotThrow();
		m_exp->setDoesNotAccessMemory();

		auto iter = m_exp->arg_begin();
		llvm::Argument* base = &(*iter++);
		base->setName("base");
		llvm::Argument* exponent = &(*iter);
		exponent->setName("exponent");

		InsertPointGuard guard{m_builder};

		//	while (e != 0) {
		//		if (e % 2 == 1)
		//			r *= b;
		//		b *= b;
		//		e /= 2;
		//	}

		auto entryBB = llvm::BasicBlock::Create(m_builder.getContext(), "Entry", m_exp);
		auto headerBB = llvm::BasicBlock::Create(m_builder.getContext(), "LoopHeader", m_exp);
		auto bodyBB = llvm::BasicBlock::Create(m_builder.getContext(), "LoopBody", m_exp);
		auto updateBB = llvm::BasicBlock::Create(m_builder.getContext(), "ResultUpdate", m_exp);
		auto continueBB = llvm::BasicBlock::Create(m_builder.getContext(), "Continue", m_exp);
		auto returnBB = llvm::BasicBlock::Create(m_builder.getContext(), "Return", m_exp);

		m_builder.SetInsertPoint(entryBB);
		m_builder.CreateBr(headerBB);

		m_builder.SetInsertPoint(headerBB);
		auto r = m_builder.CreatePHI(Type::Word, 2, "r");
		auto b = m_builder.CreatePHI(Type::Word, 2, "b");
		auto e = m_builder.CreatePHI(Type::Word, 2, "e");
		auto eNonZero = m_builder.CreateICmpNE(e, Constant::get(0), "e.nonzero");
		m_builder.CreateCondBr(eNonZero, bodyBB, returnBB);

		m_builder.SetInsertPoint(bodyBB);
		auto eOdd = m_builder.CreateICmpNE(m_builder.CreateAnd(e, Constant::get(1)), Constant::get(0), "e.isodd");
		m_builder.CreateCondBr(eOdd, updateBB, continueBB);

		m_builder.SetInsertPoint(updateBB);
		auto r0 = m_builder.CreateMul(r, b);
		m_builder.CreateBr(continueBB);

		m_builder.SetInsertPoint(continueBB);
		auto r1 = m_builder.CreatePHI(Type::Word, 2, "r1");
		r1->addIncoming(r, bodyBB);
		r1->addIncoming(r0, updateBB);
		auto b1 = m_builder.CreateMul(b, b);
		auto e1 = m_builder.CreateLShr(e, Constant::get(1), "e1");
		m_builder.CreateBr(headerBB);

		r->addIncoming(Constant::get(1), entryBB);
		r->addIncoming(r1, continueBB);
		b->addIncoming(base, entryBB);
		b->addIncoming(b1, continueBB);
		e->addIncoming(exponent, entryBB);
		e->addIncoming(e1, continueBB);

		m_builder.SetInsertPoint(returnBB);
		m_builder.CreateRet(r);
	}
	return m_exp;
}

llvm::Value* Arith256::exp(llvm::Value* _arg1, llvm::Value* _arg2)
{
	//	while (e != 0) {
	//		if (e % 2 == 1)
	//			r *= b;
	//		b *= b;
	//		e /= 2;
	//	}

	if (auto c1 = llvm::dyn_cast<llvm::ConstantInt>(_arg1))
	{
		if (auto c2 = llvm::dyn_cast<llvm::ConstantInt>(_arg2))
		{
			auto b = c1->getValue();
			auto e = c2->getValue();
			auto r = llvm::APInt{256, 1};
			while (e != 0)
			{
				if (e[0])
					r *= b;
				b *= b;
				e = e.lshr(1);
			}
			return Constant::get(r);
		}
	}

	return m_builder.CreateCall(getExpFunc(), {_arg1, _arg2});
}

}
}
}

extern "C"
{
	EXPORT void debug(uint64_t a, uint64_t b, uint64_t c, uint64_t d, char z)
	{
		DLOG(JIT) << "DEBUG " << std::dec << z << ": " //<< d << c << b << a
				<< " ["	<< std::hex << std::setfill('0') << std::setw(16) << d << std::setw(16) << c << std::setw(16) << b << std::setw(16) << a << "]\n";
	}
}
