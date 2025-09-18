import { describe, it, expect } from 'vitest';
import { TokenStream, Token, TokenType } from './compiler';

describe('TokenStream', () => {
  // Helper function to create test tokens
  const createTokens = (): Token[] => [
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
  ];

  describe('constructor', () => {
    it('should initialize with empty token array', () => {
      const stream = new TokenStream([]);
      expect(stream.isEmpty()).toBe(true);
    });

    it('should initialize with token array', () => {
      const tokens = createTokens();
      const stream = new TokenStream(tokens);
      expect(stream.isEmpty()).toBe(false);
    });
  });

  describe('takeToken', () => {
    it('should return first token and advance position', () => {
      const tokens = createTokens();
      const stream = new TokenStream(tokens);
      
      const token = stream.takeToken();
      expect(token).toEqual({ type: TokenType.KwInt });
    });

    it('should return subsequent tokens in order', () => {
      const tokens = createTokens();
      const stream = new TokenStream(tokens);
      
      expect(stream.takeToken()).toEqual({ type: TokenType.KwInt });
      expect(stream.takeToken()).toEqual({ type: TokenType.Ident, value: 'main' });
      expect(stream.takeToken()).toEqual({ type: TokenType.LParen });
    });

    it('should return null when no more tokens', () => {
      const stream = new TokenStream([]);
      expect(stream.takeToken()).toBeNull();
    });

    it('should return null after consuming all tokens', () => {
      const tokens = [{ type: TokenType.KwInt }];
      const stream = new TokenStream(tokens);
      
      expect(stream.takeToken()).toEqual({ type: TokenType.KwInt });
      expect(stream.takeToken()).toBeNull();
    });
  });

  describe('peek', () => {
    it('should return current token without advancing position', () => {
      const tokens = createTokens();
      const stream = new TokenStream(tokens);
      
      const token1 = stream.peek();
      const token2 = stream.peek();
      
      expect(token1).toEqual({ type: TokenType.KwInt });
      expect(token2).toEqual({ type: TokenType.KwInt });
      expect(token1).toEqual(token2);
    });

    it('should return null for empty stream', () => {
      const stream = new TokenStream([]);
      expect(stream.peek()).toBeNull();
    });

    it('should return null after consuming all tokens', () => {
      const tokens = [{ type: TokenType.KwInt }];
      const stream = new TokenStream(tokens);
      
      stream.takeToken(); // consume the token
      expect(stream.peek()).toBeNull();
    });

    it('should work correctly with takeToken', () => {
      const tokens = createTokens();
      const stream = new TokenStream(tokens);
      
      expect(stream.peek()).toEqual({ type: TokenType.KwInt });
      expect(stream.takeToken()).toEqual({ type: TokenType.KwInt });
      expect(stream.peek()).toEqual({ type: TokenType.Ident, value: 'main' });
    });
  });

  describe('npeek', () => {
    it('should return empty array for n=0', () => {
      const tokens = createTokens();
      const stream = new TokenStream(tokens);
      
      const result = stream.npeek(0);
      expect(result).toEqual([]);
    });

    it('should return first n tokens without advancing position', () => {
      const tokens = createTokens();
      const stream = new TokenStream(tokens);
      
      const result = stream.npeek(3);
      expect(result).toEqual([
        { type: TokenType.KwInt },
        { type: TokenType.Ident, value: 'main' },
        { type: TokenType.LParen }
      ]);
      
      // Position should not have advanced
      expect(stream.peek()).toEqual({ type: TokenType.KwInt });
    });

    it('should return available tokens when n exceeds remaining tokens', () => {
      const tokens = [
        { type: TokenType.KwInt },
        { type: TokenType.Ident, value: 'main' }
      ];
      const stream = new TokenStream(tokens);
      
      const result = stream.npeek(5);
      expect(result).toEqual([
        { type: TokenType.KwInt },
        { type: TokenType.Ident, value: 'main' }
      ]);
    });

    it('should return empty array for empty stream', () => {
      const stream = new TokenStream([]);
      const result = stream.npeek(3);
      expect(result).toEqual([]);
    });

    it('should work correctly after consuming some tokens', () => {
      const tokens = createTokens();
      const stream = new TokenStream(tokens);
      
      stream.takeToken(); // consume KwInt
      stream.takeToken(); // consume Ident
      
      const result = stream.npeek(2);
      expect(result).toEqual([
        { type: TokenType.LParen },
        { type: TokenType.KwVoid }
      ]);
    });

    it('should handle single token peek', () => {
      const tokens = createTokens();
      const stream = new TokenStream(tokens);
      
      const result = stream.npeek(1);
      expect(result).toEqual([{ type: TokenType.KwInt }]);
    });
  });

  describe('isEmpty', () => {
    it('should return true for empty stream', () => {
      const stream = new TokenStream([]);
      expect(stream.isEmpty()).toBe(true);
    });

    it('should return false for non-empty stream', () => {
      const tokens = createTokens();
      const stream = new TokenStream(tokens);
      expect(stream.isEmpty()).toBe(false);
    });

    it('should return true after consuming all tokens', () => {
      const tokens = [{ type: TokenType.KwInt }];
      const stream = new TokenStream(tokens);
      
      expect(stream.isEmpty()).toBe(false);
      stream.takeToken();
      expect(stream.isEmpty()).toBe(true);
    });

    it('should work correctly during token consumption', () => {
      const tokens = [
        { type: TokenType.KwInt },
        { type: TokenType.Ident, value: 'main' }
      ];
      const stream = new TokenStream(tokens);
      
      expect(stream.isEmpty()).toBe(false);
      stream.takeToken();
      expect(stream.isEmpty()).toBe(false);
      stream.takeToken();
      expect(stream.isEmpty()).toBe(true);
    });
  });

  describe('integration tests', () => {
    it('should handle complete token stream processing', () => {
      const tokens = [
        { type: TokenType.KwInt },
        { type: TokenType.Ident, value: 'main' },
        { type: TokenType.LParen },
        { type: TokenType.RParen }
      ];
      const stream = new TokenStream(tokens);
      
      // Process all tokens
      const processedTokens: Token[] = [];
      while (!stream.isEmpty()) {
        const token = stream.takeToken();
        if (token) {
          processedTokens.push(token);
        }
      }
      
      expect(processedTokens).toEqual(tokens);
      expect(stream.isEmpty()).toBe(true);
      expect(stream.takeToken()).toBeNull();
      expect(stream.peek()).toBeNull();
    });

    it('should support lookahead parsing patterns', () => {
      const tokens = [
        { type: TokenType.KwInt },
        { type: TokenType.Ident, value: 'main' },
        { type: TokenType.LParen },
        { type: TokenType.KwVoid },
        { type: TokenType.RParen }
      ];
      const stream = new TokenStream(tokens);
      
      // Simulate parser lookahead
      expect(stream.peek()).toEqual({ type: TokenType.KwInt });
      
      // Look ahead 2 tokens to check function signature
      const lookahead = stream.npeek(3);
      expect(lookahead[0]).toEqual({ type: TokenType.KwInt });
      expect(lookahead[1]).toEqual({ type: TokenType.Ident, value: 'main' });
      expect(lookahead[2]).toEqual({ type: TokenType.LParen });
      
      // Consume tokens after lookahead
      expect(stream.takeToken()).toEqual({ type: TokenType.KwInt });
      expect(stream.takeToken()).toEqual({ type: TokenType.Ident, value: 'main' });
      
      // Continue processing
      expect(stream.peek()).toEqual({ type: TokenType.LParen });
    });
  });
});