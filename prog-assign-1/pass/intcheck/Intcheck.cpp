#include <list>

#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Constants.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/IR/Module.h"
using namespace llvm;
using namespace std;

namespace {
  struct IntcheckPass : public FunctionPass {
    static char ID;
    LLVMContext *C;
    Constant *logFunc;
    Type *VoidTy;
    Type *Int32Ty;

    IntcheckPass() : FunctionPass(ID) {}

    bool doInitialization(Module &M) {
      C = &(M.getContext());
      VoidTy = Type::getVoidTy(*C);
      Int32Ty = Type::getInt32Ty(*C);
      logFunc = M.getOrInsertFunction("logop", VoidTy, Int32Ty, NULL);
      return true;
    }

    bool shouldCheckOverflow(Value *I, int depth) {
      // TODO: implement simple dataflow analysis to see if the computed data is
      // flowing into malloc().
      bool is_inst_overflow = false;
      auto *op = dyn_cast<BinaryOperator>(I);
      auto opcode = op->getOpcode();

      //errs() << "[DEBUG] start" << "\n";
      // FIXME: Need to consider Constant Propagation
      //  Now, let's assume the constant propagation is already applied
      switch (opcode) {
        case Instruction::Mul:
          {
            is_inst_overflow = true;
            break;
          }
        case Instruction::Add:
          {
            is_inst_overflow = true;
            break;
          }
        default:
          is_inst_overflow = false;
      }

      bool is_malloc_overflow = false;
      if (is_inst_overflow) {
        // check the Value would be used in following malloc without modification
        Value *target_val = nullptr;
        list<Value*> target_val_used;
        Function *func = op->getFunction();
        for (auto &_B : *func) {
          for (auto &_I : _B) {
            if (auto *storeInst = dyn_cast<StoreInst>(&_I)) {
              Value *src = storeInst->getOperand(0);
              Value *dst = storeInst->getOperand(1);
              if (src == I) {
                target_val = dst;
                target_val_used.push_back(target_val);  // Include target_val inself
              }
            }
            if (auto *loadInst = dyn_cast<LoadInst>(&_I)) {
              Value *src = loadInst->getOperand(0);

              for (Value *used_val : target_val_used) {
                if (used_val == src) {
                  target_val_used.push_back(&_I);
                  break;
                }
              }
            }
            if (auto *zextInst = dyn_cast<ZExtInst>(&_I)) {
              Value *src = zextInst->getOperand(0);

              for (Value *used_val : target_val_used) {
                if (used_val == src) {
                  target_val_used.push_back(&_I);
                  break;
                }
              }
            }

            // check malloc
            if (auto *callInst = dyn_cast<CallInst>(&_I)) {
              if (auto *tmpfunc = callInst->getCalledFunction()) {
                if (tmpfunc->getName() == "malloc") {
                  Value *arg1 = callInst->getOperand(0);

                  for (Value *used_val : target_val_used) {
                    if (used_val == arg1) {
                      //errs() << "OVERFLOW DETECTED!!!!\n";
                      is_malloc_overflow = true;
                      break;
                    }
                  }
                }
              }
            }
          }
        }
      }
      return is_malloc_overflow;
    }

    Value* getLineNum(Instruction *I) {
      const DebugLoc *debugLoc = &I->getDebugLoc();

      if (debugLoc)
        return ConstantInt::get(Int32Ty, debugLoc->getLine());
      return ConstantInt::get(Int32Ty, -1);
    }

    virtual bool runOnFunction(Function &F) {
      bool res = false;

      for (auto &B : F) {
        for (auto &I : B) {
          //I.dump();
          if (auto *op = dyn_cast<BinaryOperator>(&I)) {
            // TODO: Implement the shouldCheckOverflow() function.
            if (!shouldCheckOverflow(&I, 0))
              continue;

            errs() << "Instrument: " << I << "\n";

            // Insert call instruction *after* `op`.
            // TODO: Pass more information including operands of computations.
            IRBuilder<> builder(op);
            builder.SetInsertPoint(&B, ++builder.GetInsertPoint());
            Value* args[] = {op, getLineNum(&I)};
            builder.CreateCall(logFunc, args);
            res |= true;
          }
        }
      }
      return res;
    }
  };
}

char IntcheckPass::ID = 0;

// Automatically enable the pass.
// http://adriansampson.net/blog/clangpass.html
static void registerIntcheckPass(const PassManagerBuilder &,
                         legacy::PassManagerBase &PM) {
  PM.add(new IntcheckPass());
}
static RegisterStandardPasses
  RegisterMyPass(PassManagerBuilder::EP_EarlyAsPossible,
                 registerIntcheckPass);
