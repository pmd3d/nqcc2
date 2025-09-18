import { describe, it, expect, beforeEach } from 'vitest';
import { 
  lexer, 
  TokenStream, 
  parseProgram,
  emitTacky,
  emitTackyFunction,
  emitTackyStatement,
  emitTackyExpr,
  emitTackyFactor,
  resetTempCounter,
  generateTempVar,
  astUnopToTackyUnop,
  astBinopToTackyBinop,
  Type,
  Unop,
  Binop,
  UnaryOp,
  BinaryOp
} from './compiler';

describe('Tacky Code Generation', () => {
  beforeEach(() => {
    resetTempCounter();
  });

  describe('generateTempVar', () => {
    it('should generate unique temporary variable names', () => {
      const temp1 = generateTempVar();
      const temp2 = generateTempVar();
      const temp3 = generateTempVar();
      
      expect(temp1).toBe('tmp.0');
      expect(temp2).toBe('tmp.1');
      expect(temp3).toBe('tmp.2');
    });

    it('should reset counter correctly', () => {
      generateTempVar(); // tmp.0
      generateTempVar(); // tmp.1
      resetTempCounter();
      const temp = generateTempVar();
      expect(temp).toBe('tmp.0');
    });
  });

  describe('operator conversion', () => {
    it('should convert AST unary operators to tacky', () => {
      expect(astUnopToTackyUnop(Unop.Complement)).toBe(UnaryOp.Complement);
      expect(astUnopToTackyUnop(Unop.Negative)).toBe(UnaryOp.Negate);
    });

    it('should convert AST binary operators to tacky', () => {
      expect(astBinopToTackyBinop(Binop.Add)).toBe(BinaryOp.Add);
      expect(astBinopToTackyBinop(Binop.Subtract)).toBe(BinaryOp.Subtract);
      expect(astBinopToTackyBinop(Binop.Multiply)).toBe(BinaryOp.Multiply);
      expect(astBinopToTackyBinop(Binop.Divide)).toBe(BinaryOp.Divide);
      expect(astBinopToTackyBinop(Binop.Modulo)).toBe(BinaryOp.Remainder);
    });
  });

  describe('emitTackyFactor', () => {
    it('should emit constant for integer literal', () => {
      const factor = { type: 'Int' as const, value: 42 };
      const instructions: any[] = [];
      
      const result = emitTackyFactor(factor, instructions);
      
      expect(result).toEqual({ type: 'Constant', value: 42 });
      expect(instructions).toHaveLength(0);
    });

    it('should emit unary instruction for complement', () => {
      const factor = {
        type: 'UnopExpr' as const,
        value: {
          unop: Unop.Complement,
          expr: { type: 'Int' as const, value: 42 }
        }
      };
      const instructions: any[] = [];
      
      const result = emitTackyFactor(factor, instructions);
      
      expect(result).toEqual({ type: 'Var', value: 'tmp.0' });
      expect(instructions).toHaveLength(1);
      expect(instructions[0]).toEqual({
        type: 'Unary',
        value: {
          op: UnaryOp.Complement,
          src: { type: 'Constant', value: 42 },
          dst: { type: 'Var', value: 'tmp.0' }
        }
      });
    });

    it('should emit unary instruction for negation', () => {
      const factor = {
        type: 'UnopExpr' as const,
        value: {
          unop: Unop.Negative,
          expr: { type: 'Int' as const, value: 5 }
        }
      };
      const instructions: any[] = [];
      
      const result = emitTackyFactor(factor, instructions);
      
      expect(result).toEqual({ type: 'Var', value: 'tmp.0' });
      expect(instructions).toHaveLength(1);
      expect(instructions[0]).toEqual({
        type: 'Unary',
        value: {
          op: UnaryOp.Negate,
          src: { type: 'Constant', value: 5 },
          dst: { type: 'Var', value: 'tmp.0' }
        }
      });
    });

    it('should handle nested unary operators', () => {
      const factor = {
        type: 'UnopExpr' as const,
        value: {
          unop: Unop.Negative,
          expr: {
            type: 'UnopExpr' as const,
            value: {
              unop: Unop.Complement,
              expr: { type: 'Int' as const, value: 10 }
            }
          }
        }
      };
      const instructions: any[] = [];
      
      const result = emitTackyFactor(factor, instructions);
      
      expect(result).toEqual({ type: 'Var', value: 'tmp.1' });
      expect(instructions).toHaveLength(2);
      expect(instructions[0].type).toBe('Unary');
      expect(instructions[1].type).toBe('Unary');
    });
  });

  describe('emitTackyExpr', () => {
    it('should emit binary instruction for addition', () => {
      const expr = {
        type: 'Binary' as const,
        value: {
          op: Binop.Add,
          left: {
            type: 'Factor' as const,
            value: { type: 'Int' as const, value: 2 }
          },
          right: {
            type: 'Factor' as const,
            value: { type: 'Int' as const, value: 3 }
          }
        }
      };
      const instructions: any[] = [];
      
      const result = emitTackyExpr(expr, instructions);
      
      expect(result).toEqual({ type: 'Var', value: 'tmp.0' });
      expect(instructions).toHaveLength(1);
      expect(instructions[0]).toEqual({
        type: 'Binary',
        value: {
          op: BinaryOp.Add,
          src1: { type: 'Constant', value: 2 },
          src2: { type: 'Constant', value: 3 },
          dst: { type: 'Var', value: 'tmp.0' }
        }
      });
    });

    it('should emit binary instruction for subtraction', () => {
      const expr = {
        type: 'Binary' as const,
        value: {
          op: Binop.Subtract,
          left: {
            type: 'Factor' as const,
            value: { type: 'Int' as const, value: 10 }
          },
          right: {
            type: 'Factor' as const,
            value: { type: 'Int' as const, value: 4 }
          }
        }
      };
      const instructions: any[] = [];
      
      const result = emitTackyExpr(expr, instructions);
      
      expect(result).toEqual({ type: 'Var', value: 'tmp.0' });
      expect(instructions).toHaveLength(1);
      expect(instructions[0].value.op).toBe(BinaryOp.Subtract);
    });

    it('should emit binary instruction for multiplication', () => {
      const expr = {
        type: 'Binary' as const,
        value: {
          op: Binop.Multiply,
          left: {
            type: 'Factor' as const,
            value: { type: 'Int' as const, value: 3 }
          },
          right: {
            type: 'Factor' as const,
            value: { type: 'Int' as const, value: 4 }
          }
        }
      };
      const instructions: any[] = [];
      
      const result = emitTackyExpr(expr, instructions);
      
      expect(result).toEqual({ type: 'Var', value: 'tmp.0' });
      expect(instructions).toHaveLength(1);
      expect(instructions[0].value.op).toBe(BinaryOp.Multiply);
    });

    it('should emit binary instruction for division', () => {
      const expr = {
        type: 'Binary' as const,
        value: {
          op: Binop.Divide,
          left: {
            type: 'Factor' as const,
            value: { type: 'Int' as const, value: 8 }
          },
          right: {
            type: 'Factor' as const,
            value: { type: 'Int' as const, value: 2 }
          }
        }
      };
      const instructions: any[] = [];
      
      const result = emitTackyExpr(expr, instructions);
      
      expect(result).toEqual({ type: 'Var', value: 'tmp.0' });
      expect(instructions).toHaveLength(1);
      expect(instructions[0].value.op).toBe(BinaryOp.Divide);
    });

    it('should emit binary instruction for modulo', () => {
      const expr = {
        type: 'Binary' as const,
        value: {
          op: Binop.Modulo,
          left: {
            type: 'Factor' as const,
            value: { type: 'Int' as const, value: 7 }
          },
          right: {
            type: 'Factor' as const,
            value: { type: 'Int' as const, value: 3 }
          }
        }
      };
      const instructions: any[] = [];
      
      const result = emitTackyExpr(expr, instructions);
      
      expect(result).toEqual({ type: 'Var', value: 'tmp.0' });
      expect(instructions).toHaveLength(1);
      expect(instructions[0].value.op).toBe(BinaryOp.Remainder);
    });

    it('should handle complex nested expressions', () => {
      // 2 + 3 * 4
      const expr = {
        type: 'Binary' as const,
        value: {
          op: Binop.Add,
          left: {
            type: 'Factor' as const,
            value: { type: 'Int' as const, value: 2 }
          },
          right: {
            type: 'Binary' as const,
            value: {
              op: Binop.Multiply,
              left: {
                type: 'Factor' as const,
                value: { type: 'Int' as const, value: 3 }
              },
              right: {
                type: 'Factor' as const,
                value: { type: 'Int' as const, value: 4 }
              }
            }
          }
        }
      };
      const instructions: any[] = [];
      
      const result = emitTackyExpr(expr, instructions);
      
      expect(result).toEqual({ type: 'Var', value: 'tmp.1' });
      expect(instructions).toHaveLength(2);
      
      // First instruction: 3 * 4 -> tmp.0
      expect(instructions[0]).toEqual({
        type: 'Binary',
        value: {
          op: BinaryOp.Multiply,
          src1: { type: 'Constant', value: 3 },
          src2: { type: 'Constant', value: 4 },
          dst: { type: 'Var', value: 'tmp.0' }
        }
      });
      
      // Second instruction: 2 + tmp.0 -> tmp.1
      expect(instructions[1]).toEqual({
        type: 'Binary',
        value: {
          op: BinaryOp.Add,
          src1: { type: 'Constant', value: 2 },
          src2: { type: 'Var', value: 'tmp.0' },
          dst: { type: 'Var', value: 'tmp.1' }
        }
      });
    });

    it('should handle expressions with unary operators', () => {
      // -2 + ~3
      const expr = {
        type: 'Binary' as const,
        value: {
          op: Binop.Add,
          left: {
            type: 'Factor' as const,
            value: {
              type: 'UnopExpr' as const,
              value: {
                unop: Unop.Negative,
                expr: { type: 'Int' as const, value: 2 }
              }
            }
          },
          right: {
            type: 'Factor' as const,
            value: {
              type: 'UnopExpr' as const,
              value: {
                unop: Unop.Complement,
                expr: { type: 'Int' as const, value: 3 }
              }
            }
          }
        }
      };
      const instructions: any[] = [];
      
      const result = emitTackyExpr(expr, instructions);
      
      expect(result).toEqual({ type: 'Var', value: 'tmp.2' });
      expect(instructions).toHaveLength(3);
      
      // First: -2 -> tmp.0
      expect(instructions[0].type).toBe('Unary');
      expect(instructions[0].value.op).toBe(UnaryOp.Negate);
      
      // Second: ~3 -> tmp.1
      expect(instructions[1].type).toBe('Unary');
      expect(instructions[1].value.op).toBe(UnaryOp.Complement);
      
      // Third: tmp.0 + tmp.1 -> tmp.2
      expect(instructions[2].type).toBe('Binary');
      expect(instructions[2].value.op).toBe(BinaryOp.Add);
    });
  });

  describe('emitTackyStatement', () => {
    it('should emit return instruction with expression', () => {
      const stmt = {
        type: 'Return' as const,
        expr: {
          type: 'Factor' as const,
          value: { type: 'Int' as const, value: 42 }
        }
      };
      const instructions: any[] = [];
      
      emitTackyStatement(stmt, instructions);
      
      expect(instructions).toHaveLength(1);
      expect(instructions[0]).toEqual({
        type: 'Return',
        value: { type: 'Constant', value: 42 }
      });
    });

    it('should emit return instruction for void return', () => {
      const stmt = { type: 'Return' as const };
      const instructions: any[] = [];
      
      emitTackyStatement(stmt, instructions);
      
      expect(instructions).toHaveLength(1);
      expect(instructions[0]).toEqual({
        type: 'Return',
        value: { type: 'Constant', value: 0 }
      });
    });

    it('should emit return instruction with complex expression', () => {
      const stmt = {
        type: 'Return' as const,
        expr: {
          type: 'Binary' as const,
          value: {
            op: Binop.Add,
            left: {
              type: 'Factor' as const,
              value: { type: 'Int' as const, value: 2 }
            },
            right: {
              type: 'Factor' as const,
              value: { type: 'Int' as const, value: 3 }
            }
          }
        }
      };
      const instructions: any[] = [];
      
      emitTackyStatement(stmt, instructions);
      
      expect(instructions).toHaveLength(2);
      expect(instructions[0].type).toBe('Binary');
      expect(instructions[1].type).toBe('Return');
      expect(instructions[1].value).toEqual({ type: 'Var', value: 'tmp.0' });
    });
  });

  describe('emitTackyFunction', () => {
    it('should emit tacky function for simple return', () => {
      const func = {
        retType: Type.Int,
        name: 'main',
        params: [],
        body: [{
          type: 'Return' as const,
          expr: {
            type: 'Factor' as const,
            value: { type: 'Int' as const, value: 42 }
          }
        }]
      };
      
      const tackyFunc = emitTackyFunction(func);
      
      expect(tackyFunc).toEqual({
        identifier: 'main',
        body: [{
          type: 'Return',
          value: { type: 'Constant', value: 42 }
        }]
      });
    });

    it('should emit tacky function with multiple statements', () => {
      const func = {
        retType: Type.Int,
        name: 'test',
        params: [],
        body: [
          {
            type: 'Return' as const,
            expr: {
              type: 'Factor' as const,
              value: { type: 'Int' as const, value: 1 }
            }
          },
          {
            type: 'Return' as const,
            expr: {
              type: 'Factor' as const,
              value: { type: 'Int' as const, value: 2 }
            }
          }
        ]
      };
      
      const tackyFunc = emitTackyFunction(func);
      
      expect(tackyFunc.identifier).toBe('test');
      expect(tackyFunc.body).toHaveLength(2);
      expect(tackyFunc.body[0].type).toBe('Return');
      expect(tackyFunc.body[1].type).toBe('Return');
    });
  });

  describe('emitTacky', () => {
    it('should emit tacky program for simple function', () => {
      const program = {
        funcs: [{
          retType: Type.Int,
          name: 'main',
          params: [],
          body: [{
            type: 'Return' as const,
            expr: {
              type: 'Factor' as const,
              value: { type: 'Int' as const, value: 42 }
            }
          }]
        }]
      };
      
      const tackyProgram = emitTacky(program);
      
      expect(tackyProgram).toEqual({
        type: 'Program',
        value: {
          identifier: 'main',
          body: [{
            type: 'Return',
            value: { type: 'Constant', value: 42 }
          }]
        }
      });
    });

    it('should throw error for multiple functions', () => {
      const program = {
        funcs: [
          {
            retType: Type.Int,
            name: 'main',
            params: [],
            body: []
          },
          {
            retType: Type.Void,
            name: 'test',
            params: [],
            body: []
          }
        ]
      };
      
      expect(() => emitTacky(program)).toThrow('Only single-function programs are supported');
    });
  });

  describe('integration tests', () => {
    it('should generate correct tacky for simple program', () => {
      const sourceCode = 'int main(void) { return 42; }';
      const tokens = new TokenStream(lexer(sourceCode));
      const ast = parseProgram(tokens);
      
      const tackyProgram = emitTacky(ast);
      
      expect(tackyProgram.type).toBe('Program');
      expect(tackyProgram.value.identifier).toBe('main');
      expect(tackyProgram.value.body).toHaveLength(1);
      expect(tackyProgram.value.body[0]).toEqual({
        type: 'Return',
        value: { type: 'Constant', value: 42 }
      });
    });

    it('should generate correct tacky for arithmetic expression', () => {
      const sourceCode = 'int main(void) { return 2 + 3 * 4; }';
      const tokens = new TokenStream(lexer(sourceCode));
      const ast = parseProgram(tokens);
      
      const tackyProgram = emitTacky(ast);
      
      // Should have: 3*4->tmp.0, 2+tmp.0->tmp.1, return tmp.1
      expect(tackyProgram.value.body).toHaveLength(3);
      expect(tackyProgram.value.body[0].type).toBe('Binary'); // 3 * 4
      expect(tackyProgram.value.body[1].type).toBe('Binary'); // 2 + tmp.0
      expect(tackyProgram.value.body[2].type).toBe('Return'); // return tmp.1
    });

    it('should generate correct tacky for unary expression', () => {
      const sourceCode = 'int main(void) { return -~42; }';
      const tokens = new TokenStream(lexer(sourceCode));
      const ast = parseProgram(tokens);
      
      const tackyProgram = emitTacky(ast);
      
      expect(tackyProgram.value.body).toHaveLength(3);
      expect(tackyProgram.value.body[0].type).toBe('Unary');
      expect(tackyProgram.value.body[1].type).toBe('Unary');
      expect(tackyProgram.value.body[2].type).toBe('Return');
    });

    it('should generate correct tacky for complex expression', () => {
      const sourceCode = 'int main(void) { return (2 + 3) * (4 - 1); }';
      const tokens = new TokenStream(lexer(sourceCode));
      const ast = parseProgram(tokens);
      
      const tackyProgram = emitTacky(ast);
      
      // Should have: 2+3->tmp.0, 4-1->tmp.1, tmp.0*tmp.1->tmp.2, return tmp.2
      expect(tackyProgram.value.body).toHaveLength(4);
      expect(tackyProgram.value.body[0].type).toBe('Binary'); // 2 + 3
      expect(tackyProgram.value.body[1].type).toBe('Binary'); // 4 - 1
      expect(tackyProgram.value.body[2].type).toBe('Binary'); // tmp.0 * tmp.1
      expect(tackyProgram.value.body[3].type).toBe('Return');
    });

    it('should reset temp counter for each function', () => {
      const func1 = {
        retType: Type.Int,
        name: 'test1',
        params: [],
        body: [{
          type: 'Return' as const,
          expr: {
            type: 'Binary' as const,
            value: {
              op: Binop.Add,
              left: {
                type: 'Factor' as const,
                value: { type: 'Int' as const, value: 1 }
              },
              right: {
                type: 'Factor' as const,
                value: { type: 'Int' as const, value: 2 }
              }
            }
          }
        }]
      };

      const func2 = {
        retType: Type.Int,
        name: 'test2',
        params: [],
        body: [{
          type: 'Return' as const,
          expr: {
            type: 'Binary' as const,
            value: {
              op: Binop.Multiply,
              left: {
                type: 'Factor' as const,
                value: { type: 'Int' as const, value: 3 }
              },
              right: {
                type: 'Factor' as const,
                value: { type: 'Int' as const, value: 4 }
              }
            }
          }
        }]
      };

      const tacky1 = emitTackyFunction(func1);
      const tacky2 = emitTackyFunction(func2);

      // Both should use tmp.0 as their first temp variable
      expect(tacky1.body[1].value).toEqual({ type: 'Var', value: 'tmp.0' });
      expect(tacky2.body[1].value).toEqual({ type: 'Var', value: 'tmp.0' });
    });
  });
});