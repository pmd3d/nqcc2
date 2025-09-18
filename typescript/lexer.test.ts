import { describe, it, expect } from 'vitest';
import { lexer, TokenType, LexerError } from './compiler';

describe('Lexer', () => {
  describe('keywords', () => {
    it('should tokenize int keyword', () => {
      const tokens = lexer('int');
      expect(tokens).toEqual([{ type: TokenType.KwInt }]);
    });

    it('should tokenize void keyword', () => {
      const tokens = lexer('void');
      expect(tokens).toEqual([{ type: TokenType.KwVoid }]);
    });

    it('should tokenize return keyword', () => {
      const tokens = lexer('return');
      expect(tokens).toEqual([{ type: TokenType.KwReturn }]);
    });

    it('should tokenize multiple keywords', () => {
      const tokens = lexer('int void return');
      expect(tokens).toEqual([
        { type: TokenType.KwInt },
        { type: TokenType.KwVoid },
        { type: TokenType.KwReturn }
      ]);
    });
  });

  describe('identifiers', () => {
    it('should tokenize simple identifier', () => {
      const tokens = lexer('main');
      expect(tokens).toEqual([{ type: TokenType.Ident, value: 'main' }]);
    });

    it('should tokenize identifier with underscore', () => {
      const tokens = lexer('_test');
      expect(tokens).toEqual([{ type: TokenType.Ident, value: '_test' }]);
    });

    it('should tokenize identifier with numbers', () => {
      const tokens = lexer('var123');
      expect(tokens).toEqual([{ type: TokenType.Ident, value: 'var123' }]);
    });

    it('should tokenize mixed alphanumeric identifier', () => {
      const tokens = lexer('test_var_123');
      expect(tokens).toEqual([{ type: TokenType.Ident, value: 'test_var_123' }]);
    });
  });

  describe('numbers', () => {
    it('should tokenize single digit', () => {
      const tokens = lexer('5');
      expect(tokens).toEqual([{ type: TokenType.Num, value: 5 }]);
    });

    it('should tokenize multi-digit number', () => {
      const tokens = lexer('123');
      expect(tokens).toEqual([{ type: TokenType.Num, value: 123 }]);
    });

    it('should tokenize zero', () => {
      const tokens = lexer('0');
      expect(tokens).toEqual([{ type: TokenType.Num, value: 0 }]);
    });

    it('should tokenize large number', () => {
      const tokens = lexer('999999');
      expect(tokens).toEqual([{ type: TokenType.Num, value: 999999 }]);
    });
  });

  describe('operators and punctuation', () => {
    it('should tokenize parentheses', () => {
      const tokens = lexer('()');
      expect(tokens).toEqual([
        { type: TokenType.LParen },
        { type: TokenType.RParen }
      ]);
    });

    it('should tokenize braces', () => {
      const tokens = lexer('{}');
      expect(tokens).toEqual([
        { type: TokenType.LBrace },
        { type: TokenType.RBrace }
      ]);
    });

    it('should tokenize semicolon', () => {
      const tokens = lexer(';');
      expect(tokens).toEqual([{ type: TokenType.Semicolon }]);
    });

    it('should tokenize arithmetic operators', () => {
      const tokens = lexer('+ - * / %');
      expect(tokens).toEqual([
        { type: TokenType.Plus },
        { type: TokenType.Minus },
        { type: TokenType.Star },
        { type: TokenType.Slash },
        { type: TokenType.Percent }
      ]);
    });

    it('should tokenize unary operators', () => {
      const tokens = lexer('~ -');
      expect(tokens).toEqual([
        { type: TokenType.Tilde },
        { type: TokenType.Minus }
      ]);
    });

    it('should tokenize double minus', () => {
      const tokens = lexer('--');
      expect(tokens).toEqual([{ type: TokenType.DoubleMinus }]);
    });

    it('should distinguish double minus from two minus tokens', () => {
      const tokens = lexer('- -');
      expect(tokens).toEqual([
        { type: TokenType.Minus },
        { type: TokenType.Minus }
      ]);
    });
  });

  describe('whitespace handling', () => {
    it('should skip spaces', () => {
      const tokens = lexer('int main');
      expect(tokens).toEqual([
        { type: TokenType.KwInt },
        { type: TokenType.Ident, value: 'main' }
      ]);
    });

    it('should skip tabs and newlines', () => {
      const tokens = lexer('int\t\nmain');
      expect(tokens).toEqual([
        { type: TokenType.KwInt },
        { type: TokenType.Ident, value: 'main' }
      ]);
    });

    it('should handle multiple whitespace characters', () => {
      const tokens = lexer('int   \t\n  main');
      expect(tokens).toEqual([
        { type: TokenType.KwInt },
        { type: TokenType.Ident, value: 'main' }
      ]);
    });
  });

  describe('complex expressions', () => {
    it('should tokenize simple function signature', () => {
      const tokens = lexer('int main(void)');
      expect(tokens).toEqual([
        { type: TokenType.KwInt },
        { type: TokenType.Ident, value: 'main' },
        { type: TokenType.LParen },
        { type: TokenType.KwVoid },
        { type: TokenType.RParen }
      ]);
    });

    it('should tokenize return statement', () => {
      const tokens = lexer('return 42;');
      expect(tokens).toEqual([
        { type: TokenType.KwReturn },
        { type: TokenType.Num, value: 42 },
        { type: TokenType.Semicolon }
      ]);
    });

    it('should tokenize arithmetic expression', () => {
      const tokens = lexer('2 + 3 * 4');
      expect(tokens).toEqual([
        { type: TokenType.Num, value: 2 },
        { type: TokenType.Plus },
        { type: TokenType.Num, value: 3 },
        { type: TokenType.Star },
        { type: TokenType.Num, value: 4 }
      ]);
    });

    it('should tokenize parenthesized expression', () => {
      const tokens = lexer('(5 + 6) * 7');
      expect(tokens).toEqual([
        { type: TokenType.LParen },
        { type: TokenType.Num, value: 5 },
        { type: TokenType.Plus },
        { type: TokenType.Num, value: 6 },
        { type: TokenType.RParen },
        { type: TokenType.Star },
        { type: TokenType.Num, value: 7 }
      ]);
    });

    it('should tokenize unary expressions', () => {
      const tokens = lexer('-x + ~y');
      expect(tokens).toEqual([
        { type: TokenType.Minus },
        { type: TokenType.Ident, value: 'x' },
        { type: TokenType.Plus },
        { type: TokenType.Tilde },
        { type: TokenType.Ident, value: 'y' }
      ]);
    });

    it('should tokenize complete function', () => {
      const tokens = lexer('int main(void) { return 42; }');
      expect(tokens).toEqual([
        { type: TokenType.KwInt },
        { type: TokenType.Ident, value: 'main' },
        { type: TokenType.LParen },
        { type: TokenType.KwVoid },
        { type: TokenType.RParen },
        { type: TokenType.LBrace },
        { type: TokenType.KwReturn },
        { type: TokenType.Num, value: 42 },
        { type: TokenType.Semicolon },
        { type: TokenType.RBrace }
      ]);
    });
  });

  describe('error handling', () => {
    it('should handle empty input', () => {
      const tokens = lexer('');
      expect(tokens).toEqual([]);
    });

    it('should handle whitespace-only input', () => {
      const tokens = lexer('   \t\n  ');
      expect(tokens).toEqual([]);
    });

    it('should throw error for invalid character', () => {
      expect(() => lexer('@')).toThrow();
    });

    it('should throw error for invalid character in middle', () => {
      expect(() => lexer('int @ main')).toThrow();
    });

    it('should throw error for hash character', () => {
      expect(() => lexer('#')).toThrow();
    });

    it('should throw error for dollar sign', () => {
      expect(() => lexer('$')).toThrow();
    });
  });

  describe('edge cases', () => {
    it('should handle adjacent operators', () => {
      const tokens = lexer('+-*/');
      expect(tokens).toEqual([
        { type: TokenType.Plus },
        { type: TokenType.Minus },
        { type: TokenType.Star },
        { type: TokenType.Slash }
      ]);
    });

    it('should handle numbers adjacent to operators', () => {
      const tokens = lexer('123+456');
      expect(tokens).toEqual([
        { type: TokenType.Num, value: 123 },
        { type: TokenType.Plus },
        { type: TokenType.Num, value: 456 }
      ]);
    });

    it('should handle identifiers adjacent to numbers', () => {
      const tokens = lexer('abc123def');
      expect(tokens).toEqual([
        { type: TokenType.Ident, value: 'abc123def' }
      ]);
    });

    it('should handle mixed case keywords (should be identifiers)', () => {
      const tokens = lexer('Int VOID Return');
      expect(tokens).toEqual([
        { type: TokenType.Ident, value: 'Int' },
        { type: TokenType.Ident, value: 'VOID' },
        { type: TokenType.Ident, value: 'Return' }
      ]);
    });
  });
});