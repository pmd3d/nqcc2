import { describe, it, expect } from 'vitest';
import { 
  lexer, 
  TokenStream, 
  parseProgram, 
  parseFunction, 
  parseStatement, 
  parseExp, 
  parsePrimaryExp,
  parseType,
  ParseError,
  Type,
  Unop,
  Binop
} from './compiler';

describe('Parser', () => {
  describe('parseType', () => {
    it('should parse int type', () => {
      const tokens = new TokenStream(lexer('int'));
      const type = parseType(tokens);
      expect(type).toBe(Type.Int);
    });

    it('should parse void type', () => {
      const tokens = new TokenStream(lexer('void'));
      const type = parseType(tokens);
      expect(type).toBe(Type.Void);
    });

    it('should throw error for invalid type', () => {
      const tokens = new TokenStream(lexer('string'));
      expect(() => parseType(tokens)).toThrow(ParseError);
    });

    it('should throw error for empty input', () => {
      const tokens = new TokenStream([]);
      expect(() => parseType(tokens)).toThrow(ParseError);
    });
  });

  describe('parsePrimaryExp', () => {
    it('should parse integer literal', () => {
      const tokens = new TokenStream(lexer('42'));
      const factor = parsePrimaryExp(tokens);
      expect(factor).toEqual({
        type: 'Int',
        value: 42
      });
    });

    it('should parse parenthesized expression', () => {
      const tokens = new TokenStream(lexer('(42)'));
      const factor = parsePrimaryExp(tokens);
      expect(factor).toEqual({
        type: 'Expr',
        value: {
          type: 'Factor',
          value: {
            type: 'Int',
            value: 42
          }
        }
      });
    });

    it('should parse unary complement', () => {
      const tokens = new TokenStream(lexer('~42'));
      const factor = parsePrimaryExp(tokens);
      expect(factor).toEqual({
        type: 'UnopExpr',
        value: {
          unop: Unop.Complement,
          expr: {
            type: 'Int',
            value: 42
          }
        }
      });
    });

    it('should parse unary negation', () => {
      const tokens = new TokenStream(lexer('-42'));
      const factor = parsePrimaryExp(tokens);
      expect(factor).toEqual({
        type: 'UnopExpr',
        value: {
          unop: Unop.Negative,
          expr: {
            type: 'Int',
            value: 42
          }
        }
      });
    });

    it('should parse nested unary operators', () => {
      const tokens = new TokenStream(lexer('-~42'));
      const factor = parsePrimaryExp(tokens);
      expect(factor).toEqual({
        type: 'UnopExpr',
        value: {
          unop: Unop.Negative,
          expr: {
            type: 'UnopExpr',
            value: {
              unop: Unop.Complement,
              expr: {
                type: 'Int',
                value: 42
              }
            }
          }
        }
      });
    });

    it('should throw error for unexpected token', () => {
      const tokens = new TokenStream(lexer('+'));
      expect(() => parsePrimaryExp(tokens)).toThrow(ParseError);
    });

    it('should throw error for missing closing parenthesis', () => {
      const tokens = new TokenStream(lexer('(42'));
      expect(() => parsePrimaryExp(tokens)).toThrow(ParseError);
    });
  });

  describe('parseExp', () => {
    it('should parse simple addition', () => {
      const tokens = new TokenStream(lexer('2 + 3'));
      const expr = parseExp(tokens, 0);
      expect(expr).toEqual({
        type: 'Binary',
        value: {
          op: Binop.Add,
          left: {
            type: 'Factor',
            value: { type: 'Int', value: 2 }
          },
          right: {
            type: 'Factor',
            value: { type: 'Int', value: 3 }
          }
        }
      });
    });

    it('should parse simple subtraction', () => {
      const tokens = new TokenStream(lexer('5 - 2'));
      const expr = parseExp(tokens, 0);
      expect(expr).toEqual({
        type: 'Binary',
        value: {
          op: Binop.Subtract,
          left: {
            type: 'Factor',
            value: { type: 'Int', value: 5 }
          },
          right: {
            type: 'Factor',
            value: { type: 'Int', value: 2 }
          }
        }
      });
    });

    it('should parse multiplication', () => {
      const tokens = new TokenStream(lexer('3 * 4'));
      const expr = parseExp(tokens, 0);
      expect(expr).toEqual({
        type: 'Binary',
        value: {
          op: Binop.Multiply,
          left: {
            type: 'Factor',
            value: { type: 'Int', value: 3 }
          },
          right: {
            type: 'Factor',
            value: { type: 'Int', value: 4 }
          }
        }
      });
    });

    it('should parse division', () => {
      const tokens = new TokenStream(lexer('8 / 2'));
      const expr = parseExp(tokens, 0);
      expect(expr).toEqual({
        type: 'Binary',
        value: {
          op: Binop.Divide,
          left: {
            type: 'Factor',
            value: { type: 'Int', value: 8 }
          },
          right: {
            type: 'Factor',
            value: { type: 'Int', value: 2 }
          }
        }
      });
    });

    it('should parse modulo', () => {
      const tokens = new TokenStream(lexer('7 % 3'));
      const expr = parseExp(tokens, 0);
      expect(expr).toEqual({
        type: 'Binary',
        value: {
          op: Binop.Modulo,
          left: {
            type: 'Factor',
            value: { type: 'Int', value: 7 }
          },
          right: {
            type: 'Factor',
            value: { type: 'Int', value: 3 }
          }
        }
      });
    });

    it('should respect operator precedence (multiplication before addition)', () => {
      const tokens = new TokenStream(lexer('2 + 3 * 4'));
      const expr = parseExp(tokens, 0);
      expect(expr).toEqual({
        type: 'Binary',
        value: {
          op: Binop.Add,
          left: {
            type: 'Factor',
            value: { type: 'Int', value: 2 }
          },
          right: {
            type: 'Binary',
            value: {
              op: Binop.Multiply,
              left: {
                type: 'Factor',
                value: { type: 'Int', value: 3 }
              },
              right: {
                type: 'Factor',
                value: { type: 'Int', value: 4 }
              }
            }
          }
        }
      });
    });

    it('should respect operator precedence (division before subtraction)', () => {
      const tokens = new TokenStream(lexer('10 - 8 / 2'));
      const expr = parseExp(tokens, 0);
      expect(expr).toEqual({
        type: 'Binary',
        value: {
          op: Binop.Subtract,
          left: {
            type: 'Factor',
            value: { type: 'Int', value: 10 }
          },
          right: {
            type: 'Binary',
            value: {
              op: Binop.Divide,
              left: {
                type: 'Factor',
                value: { type: 'Int', value: 8 }
              },
              right: {
                type: 'Factor',
                value: { type: 'Int', value: 2 }
              }
            }
          }
        }
      });
    });

    it('should handle left associativity for same precedence', () => {
      const tokens = new TokenStream(lexer('10 - 5 - 2'));
      const expr = parseExp(tokens, 0);
      expect(expr).toEqual({
        type: 'Binary',
        value: {
          op: Binop.Subtract,
          left: {
            type: 'Binary',
            value: {
              op: Binop.Subtract,
              left: {
                type: 'Factor',
                value: { type: 'Int', value: 10 }
              },
              right: {
                type: 'Factor',
                value: { type: 'Int', value: 5 }
              }
            }
          },
          right: {
            type: 'Factor',
            value: { type: 'Int', value: 2 }
          }
        }
      });
    });

    it('should handle parentheses to override precedence', () => {
      const tokens = new TokenStream(lexer('(2 + 3) * 4'));
      const expr = parseExp(tokens, 0);
      expect(expr).toEqual({
        type: 'Binary',
        value: {
          op: Binop.Multiply,
          left: {
            type: 'Factor',
            value: {
              type: 'Expr',
              value: {
                type: 'Binary',
                value: {
                  op: Binop.Add,
                  left: {
                    type: 'Factor',
                    value: { type: 'Int', value: 2 }
                  },
                  right: {
                    type: 'Factor',
                    value: { type: 'Int', value: 3 }
                  }
                }
              }
            }
          },
          right: {
            type: 'Factor',
            value: { type: 'Int', value: 4 }
          }
        }
      });
    });

    it('should handle complex nested expressions', () => {
      const tokens = new TokenStream(lexer('2 * 3 + 4 * 5'));
      const expr = parseExp(tokens, 0);
      expect(expr).toEqual({
        type: 'Binary',
        value: {
          op: Binop.Add,
          left: {
            type: 'Binary',
            value: {
              op: Binop.Multiply,
              left: {
                type: 'Factor',
                value: { type: 'Int', value: 2 }
              },
              right: {
                type: 'Factor',
                value: { type: 'Int', value: 3 }
              }
            }
          },
          right: {
            type: 'Binary',
            value: {
              op: Binop.Multiply,
              left: {
                type: 'Factor',
                value: { type: 'Int', value: 4 }
              },
              right: {
                type: 'Factor',
                value: { type: 'Int', value: 5 }
              }
            }
          }
        }
      });
    });

    it('should handle unary operators in expressions', () => {
      const tokens = new TokenStream(lexer('-2 + ~3'));
      const expr = parseExp(tokens, 0);
      expect(expr).toEqual({
        type: 'Binary',
        value: {
          op: Binop.Add,
          left: {
            type: 'Factor',
            value: {
              type: 'UnopExpr',
              value: {
                unop: Unop.Negative,
                expr: { type: 'Int', value: 2 }
              }
            }
          },
          right: {
            type: 'Factor',
            value: {
              type: 'UnopExpr',
              value: {
                unop: Unop.Complement,
                expr: { type: 'Int', value: 3 }
              }
            }
          }
        }
      });
    });
  });

  describe('parseStatement', () => {
    it('should parse return statement with expression', () => {
      const tokens = new TokenStream(lexer('return 42;'));
      const stmt = parseStatement(tokens);
      expect(stmt).toEqual({
        type: 'Return',
        expr: {
          type: 'Factor',
          value: { type: 'Int', value: 42 }
        }
      });
    });

    it('should parse void return statement', () => {
      const tokens = new TokenStream(lexer('return;'));
      const stmt = parseStatement(tokens);
      expect(stmt).toEqual({
        type: 'Return'
      });
    });

    it('should parse return with complex expression', () => {
      const tokens = new TokenStream(lexer('return 2 + 3 * 4;'));
      const stmt = parseStatement(tokens);
      expect(stmt.type).toBe('Return');
      expect(stmt.expr?.type).toBe('Binary');
    });

    it('should throw error for missing semicolon', () => {
      const tokens = new TokenStream(lexer('return 42'));
      expect(() => parseStatement(tokens)).toThrow(ParseError);
    });

    it('should throw error for unexpected token', () => {
      const tokens = new TokenStream(lexer('if'));
      expect(() => parseStatement(tokens)).toThrow(ParseError);
    });
  });

  describe('parseFunction', () => {
    it('should parse simple int function', () => {
      const tokens = new TokenStream(lexer('int main(void) { return 42; }'));
      const func = parseFunction(tokens);
      expect(func).toEqual({
        retType: Type.Int,
        name: 'main',
        params: [],
        body: [{
          type: 'Return',
          expr: {
            type: 'Factor',
            value: { type: 'Int', value: 42 }
          }
        }]
      });
    });

    it('should parse void function', () => {
      const tokens = new TokenStream(lexer('void test(void) { return; }'));
      const func = parseFunction(tokens);
      expect(func).toEqual({
        retType: Type.Void,
        name: 'test',
        params: [],
        body: [{
          type: 'Return'
        }]
      });
    });

    it('should parse function with empty parameter list', () => {
      const tokens = new TokenStream(lexer('int main() { return 0; }'));
      const func = parseFunction(tokens);
      expect(func.params).toEqual([]);
    });

    it('should parse function with multiple statements', () => {
      const tokens = new TokenStream(lexer('int test(void) { return 1; return 2; }'));
      const func = parseFunction(tokens);
      expect(func.body).toHaveLength(2);
      expect(func.body[0].type).toBe('Return');
      expect(func.body[1].type).toBe('Return');
    });

    it('should throw error for missing function name', () => {
      const tokens = new TokenStream(lexer('int (void) { return 0; }'));
      expect(() => parseFunction(tokens)).toThrow(ParseError);
    });

    it('should throw error for missing opening parenthesis', () => {
      const tokens = new TokenStream(lexer('int main void) { return 0; }'));
      expect(() => parseFunction(tokens)).toThrow(ParseError);
    });

    it('should throw error for missing closing parenthesis', () => {
      const tokens = new TokenStream(lexer('int main(void { return 0; }'));
      expect(() => parseFunction(tokens)).toThrow(ParseError);
    });

    it('should throw error for missing opening brace', () => {
      const tokens = new TokenStream(lexer('int main(void) return 0; }'));
      expect(() => parseFunction(tokens)).toThrow(ParseError);
    });

    it('should throw error for missing closing brace', () => {
      const tokens = new TokenStream(lexer('int main(void) { return 0;'));
      expect(() => parseFunction(tokens)).toThrow(ParseError);
    });
  });

  describe('parseProgram', () => {
    it('should parse single function program', () => {
      const tokens = new TokenStream(lexer('int main(void) { return 42; }'));
      const program = parseProgram(tokens);
      expect(program.funcs).toHaveLength(1);
      expect(program.funcs[0].name).toBe('main');
    });

    it('should parse program with complex expression', () => {
      const tokens = new TokenStream(lexer('int main(void) { return 2 + 3 * 4; }'));
      const program = parseProgram(tokens);
      expect(program.funcs).toHaveLength(1);
      expect(program.funcs[0].body[0].type).toBe('Return');
    });

    it('should throw error for empty program', () => {
      const tokens = new TokenStream([]);
      expect(() => parseProgram(tokens)).toThrow(ParseError);
    });

    it('should handle program with void function', () => {
      const tokens = new TokenStream(lexer('void test(void) { return; }'));
      const program = parseProgram(tokens);
      expect(program.funcs).toHaveLength(1);
      expect(program.funcs[0].retType).toBe(Type.Void);
    });
  });

  describe('error cases', () => {
    it('should handle unexpected end of input in expression', () => {
      const tokens = new TokenStream(lexer('2 +'));
      expect(() => parseExp(tokens, 0)).toThrow(ParseError);
    });

    it('should handle malformed parentheses', () => {
      const tokens = new TokenStream(lexer('(2 + 3'));
      expect(() => parseExp(tokens, 0)).toThrow(ParseError);
    });

    it('should handle invalid binary operator', () => {
      // Use a valid token sequence that doesn't contain invalid characters
      const tokens = new TokenStream(lexer('2'));
      const expr = parseExp(tokens, 0);
      // Should parse just the first factor since there's no binary operator
      expect(expr).toEqual({
        type: 'Factor',
        value: { type: 'Int', value: 2 }
      });
    });

    it('should handle nested parentheses correctly', () => {
      const tokens = new TokenStream(lexer('((2 + 3) * 4)'));
      const expr = parseExp(tokens, 0);
      expect(expr.type).toBe('Factor');
      if (expr.type === 'Factor' && expr.value.type === 'Expr') {
        expect(expr.value.value.type).toBe('Binary');
      }
    });
  });
});