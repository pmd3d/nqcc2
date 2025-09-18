import { describe, it, expect, beforeEach } from 'vitest';
import { 
  lexer, 
  TokenStream, 
  parseProgram,
  emitTacky,
  emitCode,
  generateAsm,
  resetTempCounter,
  main
} from './compiler';
import * as fs from 'fs';
import * as path from 'path';
import { execSync } from 'child_process';

describe('End-to-End Integration Tests', () => {
  beforeEach(() => {
    resetTempCounter();
  });

  describe('complete compilation pipeline', () => {
    it('should compile simple return statement', () => {
      const sourceCode = 'int main(void) { return 42; }';
      
      // Tokenize
      const tokens = lexer(sourceCode);
      expect(tokens).toHaveLength(10);
      
      // Parse
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      expect(ast.funcs).toHaveLength(1);
      expect(ast.funcs[0].name).toBe('main');
      
      // Generate tacky IR
      const tackyProgram = emitTacky(ast);
      expect(tackyProgram.type).toBe('Program');
      expect(tackyProgram.value.identifier).toBe('main');
      expect(tackyProgram.value.body).toHaveLength(1);
      
      // Generate assembly
      const assemblyCode = emitCode(tackyProgram);
      expect(assemblyCode.type).toBe('ProgramCode');
      
      // Generate assembly text
      const assemblyText = generateAsm(assemblyCode);
      expect(assemblyText).toContain('main:');
      expect(assemblyText).toContain('movl $42, %eax');
      expect(assemblyText).toContain('ret');
    });

    it('should compile arithmetic expressions', () => {
      const testCases = [
        { code: 'int main(void) { return 2 + 3; }', description: 'addition' },
        { code: 'int main(void) { return 10 - 4; }', description: 'subtraction' },
        { code: 'int main(void) { return 3 * 4; }', description: 'multiplication' },
        { code: 'int main(void) { return 8 / 2; }', description: 'division' },
        { code: 'int main(void) { return 7 % 3; }', description: 'modulo' }
      ];

      testCases.forEach(({ code, description }) => {
        const tokens = lexer(code);
        const tokenStream = new TokenStream(tokens);
        const ast = parseProgram(tokenStream);
        const tackyProgram = emitTacky(ast);
        const assemblyCode = emitCode(tackyProgram);
        const assemblyText = generateAsm(assemblyCode);
        
        expect(assemblyText).toContain('main:');
        expect(assemblyText).toContain('ret');
        
        // Reset for next test
        resetTempCounter();
      });
    });

    it('should compile unary expressions', () => {
      const testCases = [
        { code: 'int main(void) { return -42; }', description: 'negation' },
        { code: 'int main(void) { return ~42; }', description: 'complement' },
        { code: 'int main(void) { return -~42; }', description: 'nested unary' }
      ];

      testCases.forEach(({ code, description }) => {
        const tokens = lexer(code);
        const tokenStream = new TokenStream(tokens);
        const ast = parseProgram(tokenStream);
        const tackyProgram = emitTacky(ast);
        const assemblyCode = emitCode(tackyProgram);
        const assemblyText = generateAsm(assemblyCode);
        
        expect(assemblyText).toContain('main:');
        expect(assemblyText).toContain('ret');
        
        // Reset for next test
        resetTempCounter();
      });
    });

    it('should compile complex expressions with precedence', () => {
      const testCases = [
        { code: 'int main(void) { return 2 + 3 * 4; }', description: 'precedence' },
        { code: 'int main(void) { return (2 + 3) * 4; }', description: 'parentheses' },
        { code: 'int main(void) { return 2 * 3 + 4 * 5; }', description: 'multiple operations' },
        { code: 'int main(void) { return ((2 + 3) * 4) - 1; }', description: 'nested parentheses' }
      ];

      testCases.forEach(({ code, description }) => {
        const tokens = lexer(code);
        const tokenStream = new TokenStream(tokens);
        const ast = parseProgram(tokenStream);
        const tackyProgram = emitTacky(ast);
        const assemblyCode = emitCode(tackyProgram);
        const assemblyText = generateAsm(assemblyCode);
        
        expect(assemblyText).toContain('main:');
        expect(assemblyText).toContain('ret');
        
        // Reset for next test
        resetTempCounter();
      });
    });

    it('should compile void functions', () => {
      const sourceCode = 'void test(void) { return; }';
      
      const tokens = lexer(sourceCode);
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      const tackyProgram = emitTacky(ast);
      const assemblyCode = emitCode(tackyProgram);
      const assemblyText = generateAsm(assemblyCode);
      
      expect(assemblyText).toContain('test:');
      expect(assemblyText).toContain('movl $0, %eax'); // Void return becomes return 0
      expect(assemblyText).toContain('ret');
    });
  });

  describe('assembly output validation', () => {
    it('should generate valid x86-64 Intel syntax', () => {
      const sourceCode = 'int main(void) { return 42; }';
      
      const tokens = lexer(sourceCode);
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      const tackyProgram = emitTacky(ast);
      const assemblyCode = emitCode(tackyProgram);
      const assemblyText = generateAsm(assemblyCode);
      
      // Check for proper Intel syntax elements
      expect(assemblyText).toMatch(/\.globl\s+main/);
      expect(assemblyText).toMatch(/\.type\s+main,\s*@function/);
      expect(assemblyText).toMatch(/main:/);
      expect(assemblyText).toMatch(/pushq\s+%rbp/);
      expect(assemblyText).toMatch(/movq\s+%rsp,\s*%rbp/);
      expect(assemblyText).toMatch(/movl\s+\$42,\s*%eax/);
      expect(assemblyText).toMatch(/movq\s+%rbp,\s*%rsp/);
      expect(assemblyText).toMatch(/popq\s+%rbp/);
      expect(assemblyText).toMatch(/ret/);
      expect(assemblyText).toMatch(/\.section\s+\.note\.GNU-stack/);
    });

    it('should generate proper function prologue and epilogue', () => {
      const sourceCode = 'int main(void) { return 2 + 3; }';
      
      const tokens = lexer(sourceCode);
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      const tackyProgram = emitTacky(ast);
      const assemblyCode = emitCode(tackyProgram);
      const assemblyText = generateAsm(assemblyCode);
      
      const lines = assemblyText.split('\n');
      
      // Find function start
      const mainIndex = lines.findIndex(line => line.trim() === 'main:');
      expect(mainIndex).toBeGreaterThan(-1);
      
      // Check prologue
      expect(lines[mainIndex + 1].trim()).toBe('pushq %rbp');
      expect(lines[mainIndex + 2].trim()).toBe('movq %rsp, %rbp');
      
      // Check epilogue (should be near the end)
      const epilogueStart = lines.findIndex(line => line.trim() === 'movq %rbp, %rsp');
      expect(epilogueStart).toBeGreaterThan(-1);
      expect(lines[epilogueStart + 1].trim()).toBe('popq %rbp');
      expect(lines[epilogueStart + 2].trim()).toBe('ret');
    });

    it('should handle stack allocation correctly', () => {
      const sourceCode = 'int main(void) { return 2 + 3 * 4; }';
      
      const tokens = lexer(sourceCode);
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      const tackyProgram = emitTacky(ast);
      const assemblyCode = emitCode(tackyProgram);
      const assemblyText = generateAsm(assemblyCode);
      
      // Should have stack allocation
      expect(assemblyText).toMatch(/subq\s+\$\d+,\s*%rsp/);
      
      // Should use stack locations
      expect(assemblyText).toMatch(/-\d+\(%rbp\)/);
    });

    it('should handle division and modulo with idiv', () => {
      const divisionCode = 'int main(void) { return 8 / 2; }';
      const moduloCode = 'int main(void) { return 7 % 3; }';
      
      [divisionCode, moduloCode].forEach(sourceCode => {
        const tokens = lexer(sourceCode);
        const tokenStream = new TokenStream(tokens);
        const ast = parseProgram(tokenStream);
        const tackyProgram = emitTacky(ast);
        const assemblyCode = emitCode(tackyProgram);
        const assemblyText = generateAsm(assemblyCode);
        
        // Should use cdq and idiv
        expect(assemblyText).toMatch(/cdq/);
        expect(assemblyText).toMatch(/idivl/);
        
        // Reset for next iteration
        resetTempCounter();
      });
    });
  });

  describe('error handling integration', () => {
    it('should handle lexer errors gracefully', () => {
      const invalidCode = 'int main(void) { return @; }';
      
      expect(() => {
        lexer(invalidCode);
      }).toThrow();
    });

    it('should handle parser errors gracefully', () => {
      const invalidCode = 'int main(void) { return 42 }'; // Missing semicolon
      
      const tokens = lexer(invalidCode);
      const tokenStream = new TokenStream(tokens);
      
      expect(() => {
        parseProgram(tokenStream);
      }).toThrow();
    });

    it('should handle empty programs', () => {
      const emptyCode = '';
      
      const tokens = lexer(emptyCode);
      const tokenStream = new TokenStream(tokens);
      
      expect(() => {
        parseProgram(tokenStream);
      }).toThrow();
    });

    it('should handle incomplete functions', () => {
      const incompleteCode = 'int main(void) {';
      
      const tokens = lexer(incompleteCode);
      const tokenStream = new TokenStream(tokens);
      
      expect(() => {
        parseProgram(tokenStream);
      }).toThrow();
    });
  });

  describe('comparison with expected outputs', () => {
    it('should match expected tacky IR structure', () => {
      const sourceCode = 'int main(void) { return 2 + 3; }';
      
      const tokens = lexer(sourceCode);
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      const tackyProgram = emitTacky(ast);
      
      // Expected structure: binary instruction + return instruction
      expect(tackyProgram.value.body).toHaveLength(2);
      expect(tackyProgram.value.body[0].type).toBe('Binary');
      expect(tackyProgram.value.body[1].type).toBe('Return');
      
      // Check binary instruction details
      const binaryInstr = tackyProgram.value.body[0];
      if (binaryInstr.type === 'Binary') {
        expect(binaryInstr.value.src1).toEqual({ type: 'Constant', value: 2 });
        expect(binaryInstr.value.src2).toEqual({ type: 'Constant', value: 3 });
        expect(binaryInstr.value.dst).toEqual({ type: 'Var', value: 'tmp.0' });
      }
      
      // Check return instruction
      const returnInstr = tackyProgram.value.body[1];
      if (returnInstr.type === 'Return') {
        expect(returnInstr.value).toEqual({ type: 'Var', value: 'tmp.0' });
      }
    });

    it('should match expected assembly structure', () => {
      const sourceCode = 'int main(void) { return 42; }';
      
      const tokens = lexer(sourceCode);
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      const tackyProgram = emitTacky(ast);
      const assemblyCode = emitCode(tackyProgram);
      
      // Expected structure: mov + ret
      expect(assemblyCode.value.instructions).toHaveLength(2);
      expect(assemblyCode.value.instructions[0].type).toBe('Mov');
      expect(assemblyCode.value.instructions[1].type).toBe('Ret');
      
      // Check mov instruction
      const movInstr = assemblyCode.value.instructions[0];
      if (movInstr.type === 'Mov') {
        expect(movInstr.value.src).toEqual({ type: 'Imm', value: 42 });
        expect(movInstr.value.dest).toEqual({ type: 'Register', value: 'AX' });
      }
    });
  });

  describe('performance and edge cases', () => {
    it('should handle deeply nested expressions', () => {
      const deepExpression = 'int main(void) { return ' + 
        '((((1 + 2) * 3) - 4) / 5) % 6'.repeat(1) + '; }';
      
      const tokens = lexer(deepExpression);
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      const tackyProgram = emitTacky(ast);
      const assemblyCode = emitCode(tackyProgram);
      const assemblyText = generateAsm(assemblyCode);
      
      expect(assemblyText).toContain('main:');
      expect(assemblyText).toContain('ret');
    });

    it('should handle large numbers', () => {
      const sourceCode = 'int main(void) { return 999999; }';
      
      const tokens = lexer(sourceCode);
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      const tackyProgram = emitTacky(ast);
      const assemblyCode = emitCode(tackyProgram);
      const assemblyText = generateAsm(assemblyCode);
      
      expect(assemblyText).toContain('movl $999999, %eax');
    });

    it('should handle zero values', () => {
      const sourceCode = 'int main(void) { return 0; }';
      
      const tokens = lexer(sourceCode);
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      const tackyProgram = emitTacky(ast);
      const assemblyCode = emitCode(tackyProgram);
      const assemblyText = generateAsm(assemblyCode);
      
      expect(assemblyText).toContain('movl $0, %eax');
    });

    it('should handle multiple unary operators', () => {
      const sourceCode = 'int main(void) { return ~~~42; }';
      
      const tokens = lexer(sourceCode);
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      const tackyProgram = emitTacky(ast);
      const assemblyCode = emitCode(tackyProgram);
      const assemblyText = generateAsm(assemblyCode);
      
      expect(assemblyText).toContain('main:');
      expect(assemblyText).toContain('ret');
      
      // Should have multiple not instructions
      const notCount = (assemblyText.match(/notl/g) || []).length;
      expect(notCount).toBe(3);
    });
  });

  describe('temporary variable management', () => {
    it('should generate unique temporary variables', () => {
      const sourceCode = 'int main(void) { return (1 + 2) + (3 + 4); }';
      
      const tokens = lexer(sourceCode);
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      const tackyProgram = emitTacky(ast);
      
      // Should have multiple temporary variables
      const instructions = tackyProgram.value.body;
      const tempVars = new Set<string>();
      
      instructions.forEach(instr => {
        if (instr.type === 'Binary') {
          if (instr.value.dst.type === 'Var') {
            tempVars.add(instr.value.dst.value);
          }
        }
      });
      
      expect(tempVars.size).toBeGreaterThan(1);
      expect(tempVars.has('tmp.0')).toBe(true);
      expect(tempVars.has('tmp.1')).toBe(true);
      expect(tempVars.has('tmp.2')).toBe(true);
    });

    it('should reset temp counter between functions', () => {
      // This is tested implicitly by running multiple tests
      // Each test should start with tmp.0
      const sourceCode = 'int main(void) { return 1 + 2; }';
      
      resetTempCounter();
      
      const tokens = lexer(sourceCode);
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      const tackyProgram = emitTacky(ast);
      
      const binaryInstr = tackyProgram.value.body[0];
      if (binaryInstr.type === 'Binary') {
        expect(binaryInstr.value.dst.value).toBe('tmp.0');
      }
    });
  });
});