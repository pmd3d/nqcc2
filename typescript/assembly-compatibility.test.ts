import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { execSync } from 'child_process';
import * as fs from 'fs';
import * as path from 'path';
import { 
  lexer, 
  TokenStream, 
  parseProgram,
  emitTacky,
  emitCode,
  generateAsm,
  resetTempCounter
} from './compiler';

describe('Assembly Output Compatibility Tests', () => {
  const tempDir = './temp_test_files';
  const testFiles: string[] = [];

  beforeEach(() => {
    resetTempCounter();
    // Create temp directory if it doesn't exist
    if (!fs.existsSync(tempDir)) {
      fs.mkdirSync(tempDir, { recursive: true });
    }
  });

  afterEach(() => {
    // Clean up test files
    testFiles.forEach(file => {
      try {
        if (fs.existsSync(file)) {
          fs.unlinkSync(file);
        }
      } catch (error) {
        // Ignore cleanup errors
      }
    });
    testFiles.length = 0;
  });

  function compileAndTest(sourceCode: string, expectedExitCode: number, testName: string) {
    const sourceFile = path.join(tempDir, `${testName}.c`);
    const assemblyFile = path.join(tempDir, `${testName}.s`);
    const executableFile = path.join(tempDir, testName);
    
    testFiles.push(sourceFile, assemblyFile, executableFile);

    // Write source file
    fs.writeFileSync(sourceFile, sourceCode);

    // Compile with our TypeScript compiler
    const tokens = lexer(sourceCode);
    const tokenStream = new TokenStream(tokens);
    const ast = parseProgram(tokenStream);
    const tackyProgram = emitTacky(ast);
    const assemblyCode = emitCode(tackyProgram);
    const assemblyText = generateAsm(assemblyCode);

    // Write assembly file
    fs.writeFileSync(assemblyFile, assemblyText);

    // Test 1: Assembly should be valid and assemblable with gcc
    expect(() => {
      execSync(`gcc -o ${executableFile} ${assemblyFile}`, { stdio: 'pipe' });
    }).not.toThrow();

    // Test 2: Executable should exist
    expect(fs.existsSync(executableFile)).toBe(true);

    // Test 3: Program should execute and return expected exit code
    try {
      execSync(executableFile, { stdio: 'pipe' });
      // If we reach here, exit code was 0
      expect(expectedExitCode).toBe(0);
    } catch (error: any) {
      // Non-zero exit code
      const actualExitCode = error.status;
      expect(actualExitCode).toBe(expectedExitCode);
    }

    return assemblyText;
  }

  describe('basic assembly generation and execution', () => {
    it('should compile and execute simple return statement', () => {
      const sourceCode = 'int main(void) { return 42; }';
      const assembly = compileAndTest(sourceCode, 42, 'simple_return');
      
      // Verify assembly structure
      expect(assembly).toContain('.globl main');
      expect(assembly).toContain('.type main, @function');
      expect(assembly).toContain('main:');
      expect(assembly).toContain('pushq %rbp');
      expect(assembly).toContain('movq %rsp, %rbp');
      expect(assembly).toContain('movl $42, %eax');
      expect(assembly).toContain('movq %rbp, %rsp');
      expect(assembly).toContain('popq %rbp');
      expect(assembly).toContain('ret');
      expect(assembly).toContain('.section .note.GNU-stack,"",@progbits');
    });

    it('should compile and execute zero return', () => {
      const sourceCode = 'int main(void) { return 0; }';
      compileAndTest(sourceCode, 0, 'zero_return');
    });

    it('should compile and execute large return value', () => {
      const sourceCode = 'int main(void) { return 255; }';
      compileAndTest(sourceCode, 255, 'large_return');
    });

    it('should compile and execute void function', () => {
      const sourceCode = 'void main(void) { return; }';
      compileAndTest(sourceCode, 0, 'void_return');
    });
  });

  describe('arithmetic expressions', () => {
    it('should compile and execute addition', () => {
      const sourceCode = 'int main(void) { return 2 + 3; }';
      compileAndTest(sourceCode, 5, 'addition');
    });

    it('should compile and execute subtraction', () => {
      const sourceCode = 'int main(void) { return 10 - 4; }';
      compileAndTest(sourceCode, 6, 'subtraction');
    });

    it('should compile and execute multiplication', () => {
      const sourceCode = 'int main(void) { return 3 * 4; }';
      compileAndTest(sourceCode, 12, 'multiplication');
    });

    it('should compile and execute division', () => {
      const sourceCode = 'int main(void) { return 8 / 2; }';
      const assembly = compileAndTest(sourceCode, 4, 'division');
      
      // Verify division uses idiv instruction
      expect(assembly).toContain('cdq');
      expect(assembly).toContain('idivl');
    });

    it('should compile and execute modulo', () => {
      const sourceCode = 'int main(void) { return 7 % 3; }';
      const assembly = compileAndTest(sourceCode, 1, 'modulo');
      
      // Verify modulo uses idiv instruction
      expect(assembly).toContain('cdq');
      expect(assembly).toContain('idivl');
    });

    it('should compile and execute complex arithmetic', () => {
      const sourceCode = 'int main(void) { return 2 + 3 * 4; }';
      compileAndTest(sourceCode, 14, 'complex_arithmetic');
    });

    it('should compile and execute parenthesized expressions', () => {
      const sourceCode = 'int main(void) { return (2 + 3) * 4; }';
      compileAndTest(sourceCode, 20, 'parentheses');
    });
  });

  describe('unary expressions', () => {
    it('should compile and execute negation', () => {
      const sourceCode = 'int main(void) { return -42; }';
      compileAndTest(sourceCode, 214, 'negation'); // -42 as unsigned 8-bit = 214
    });

    it('should compile and execute complement', () => {
      const sourceCode = 'int main(void) { return ~0; }';
      compileAndTest(sourceCode, 255, 'complement'); // ~0 as unsigned 8-bit = 255
    });

    it('should compile and execute double negation', () => {
      const sourceCode = 'int main(void) { return -(-42); }';
      compileAndTest(sourceCode, 42, 'double_negation');
    });

    it('should compile and execute nested unary operations', () => {
      const sourceCode = 'int main(void) { return -~42; }';
      const assembly = compileAndTest(sourceCode, 43, 'nested_unary');
      
      // Verify both unary operations are present
      expect(assembly).toContain('notl');
      expect(assembly).toContain('negl');
    });
  });

  describe('x86-64 AT&T syntax compliance', () => {
    it('should use correct register names', () => {
      const sourceCode = 'int main(void) { return 42; }';
      const assembly = compileAndTest(sourceCode, 42, 'register_names');
      
      // Check for proper AT&T syntax register names
      expect(assembly).toMatch(/%eax/);
      expect(assembly).toMatch(/%rbp/);
      expect(assembly).toMatch(/%rsp/);
    });

    it('should use correct immediate value syntax', () => {
      const sourceCode = 'int main(void) { return 123; }';
      const assembly = compileAndTest(sourceCode, 123, 'immediate_syntax');
      
      // Check for proper AT&T syntax immediate values
      expect(assembly).toMatch(/\$123/);
    });

    it('should use correct memory operand syntax', () => {
      const sourceCode = 'int main(void) { return 2 + 3; }';
      const assembly = compileAndTest(sourceCode, 5, 'memory_syntax');
      
      // Check for proper AT&T syntax memory operands
      expect(assembly).toMatch(/-\d+\(%rbp\)/);
    });

    it('should use correct instruction format', () => {
      const sourceCode = 'int main(void) { return 2 + 3; }';
      const assembly = compileAndTest(sourceCode, 5, 'instruction_format');
      
      // Check for proper AT&T syntax instruction format (source, destination)
      expect(assembly).toMatch(/movl\s+.*,\s*%eax/); // Final result to %eax
      expect(assembly).toMatch(/addl\s+\$\d+,\s*-\d+\(%rbp\)/); // Addition operation
    });
  });

  describe('stack management', () => {
    it('should properly allocate stack space', () => {
      const sourceCode = 'int main(void) { return 2 + 3 * 4; }';
      const assembly = compileAndTest(sourceCode, 14, 'stack_allocation');
      
      // Should have stack allocation
      expect(assembly).toMatch(/subq\s+\$\d+,\s*%rsp/);
    });

    it('should align stack to 16-byte boundary', () => {
      const sourceCode = 'int main(void) { return 1 + 2; }';
      const assembly = compileAndTest(sourceCode, 3, 'stack_alignment');
      
      // Stack allocation should be aligned to 16 bytes
      const stackMatch = assembly.match(/subq\s+\$(\d+),\s*%rsp/);
      if (stackMatch) {
        const stackSize = parseInt(stackMatch[1]);
        expect(stackSize % 16).toBe(0);
      }
    });

    it('should handle multiple temporary variables', () => {
      const sourceCode = 'int main(void) { return (1 + 2) + (3 + 4); }';
      const assembly = compileAndTest(sourceCode, 10, 'multiple_temps');
      
      // Should use multiple stack locations
      const stackMatches = assembly.match(/-\d+\(%rbp\)/g);
      expect(stackMatches).toBeTruthy();
      expect(stackMatches!.length).toBeGreaterThan(1);
    });
  });

  describe('division and modulo special handling', () => {
    it('should handle division with proper register setup', () => {
      const sourceCode = 'int main(void) { return 15 / 3; }';
      const assembly = compileAndTest(sourceCode, 5, 'division_registers');
      
      // Should move dividend to AX, sign-extend, then divide
      expect(assembly).toMatch(/movl\s+\$15,\s*%eax/);
      expect(assembly).toContain('cdq');
      expect(assembly).toMatch(/idivl/);
    });

    it('should handle modulo with proper result extraction', () => {
      const sourceCode = 'int main(void) { return 17 % 5; }';
      const assembly = compileAndTest(sourceCode, 2, 'modulo_result');
      
      // Should use DX register for remainder
      expect(assembly).toContain('cdq');
      expect(assembly).toMatch(/idivl/);
      expect(assembly).toMatch(/movl\s+%edx/);
    });

    it('should handle complex division expressions', () => {
      const sourceCode = 'int main(void) { return 15 / 3 + 17 % 5; }';
      compileAndTest(sourceCode, 7, 'complex_division'); // 5 + 2 = 7
    });
  });

  describe('function prologue and epilogue', () => {
    it('should have proper function prologue', () => {
      const sourceCode = 'int main(void) { return 42; }';
      const assembly = compileAndTest(sourceCode, 42, 'prologue');
      
      const lines = assembly.split('\n');
      const mainIndex = lines.findIndex(line => line.trim() === 'main:');
      
      expect(mainIndex).toBeGreaterThan(-1);
      expect(lines[mainIndex + 1].trim()).toBe('pushq %rbp');
      expect(lines[mainIndex + 2].trim()).toBe('movq %rsp, %rbp');
    });

    it('should have proper function epilogue', () => {
      const sourceCode = 'int main(void) { return 42; }';
      const assembly = compileAndTest(sourceCode, 42, 'epilogue');
      
      const lines = assembly.split('\n');
      const epilogueStart = lines.findIndex(line => line.trim() === 'movq %rbp, %rsp');
      
      expect(epilogueStart).toBeGreaterThan(-1);
      expect(lines[epilogueStart + 1].trim()).toBe('popq %rbp');
      expect(lines[epilogueStart + 2].trim()).toBe('ret');
    });

    it('should include GNU stack note section', () => {
      const sourceCode = 'int main(void) { return 42; }';
      const assembly = compileAndTest(sourceCode, 42, 'gnu_stack');
      
      expect(assembly).toContain('.section .note.GNU-stack,"",@progbits');
    });
  });

  describe('edge cases and error conditions', () => {
    it('should handle maximum positive return value', () => {
      const sourceCode = 'int main(void) { return 127; }';
      compileAndTest(sourceCode, 127, 'max_positive');
    });

    it('should handle negative return values', () => {
      const sourceCode = 'int main(void) { return -1; }';
      compileAndTest(sourceCode, 255, 'negative_return'); // -1 as unsigned 8-bit = 255
    });

    it('should handle zero division (runtime behavior)', () => {
      // Note: This will cause a runtime error, but assembly should be valid
      const sourceCode = 'int main(void) { return 5 / 0; }';
      const sourceFile = path.join(tempDir, 'zero_division.c');
      const assemblyFile = path.join(tempDir, 'zero_division.s');
      const executableFile = path.join(tempDir, 'zero_division');
      
      testFiles.push(sourceFile, assemblyFile, executableFile);

      fs.writeFileSync(sourceFile, sourceCode);

      const tokens = lexer(sourceCode);
      const tokenStream = new TokenStream(tokens);
      const ast = parseProgram(tokenStream);
      const tackyProgram = emitTacky(ast);
      const assemblyCode = emitCode(tackyProgram);
      const assemblyText = generateAsm(assemblyCode);

      fs.writeFileSync(assemblyFile, assemblyText);

      // Assembly should be valid and assemblable
      expect(() => {
        execSync(`gcc -o ${executableFile} ${assemblyFile}`, { stdio: 'pipe' });
      }).not.toThrow();

      expect(fs.existsSync(executableFile)).toBe(true);
      
      // Program will crash at runtime with SIGFPE, but that's expected
      // We just verify the assembly is syntactically correct
    });
  });

  describe('comparison with existing test files', () => {
    it('should match output format of test_simple.s', () => {
      const sourceCode = 'int main(void) {\n    return 42;\n}';
      const assembly = compileAndTest(sourceCode, 42, 'compare_simple');
      
      // Read the reference file
      const referenceAssembly = fs.readFileSync('test_simple.s', 'utf8');
      
      // Both should have the same key elements
      expect(assembly).toContain('.globl main');
      expect(assembly).toContain('movl $42, %eax');
      expect(assembly).toContain('.section .note.GNU-stack');
      
      // Structure should be similar (allowing for minor formatting differences)
      const assemblyLines = assembly.split('\n').filter(line => line.trim());
      const referenceLines = referenceAssembly.split('\n').filter(line => line.trim());
      
      // Should have similar number of lines
      expect(Math.abs(assemblyLines.length - referenceLines.length)).toBeLessThanOrEqual(2);
    });

    it('should handle complex expressions like test_complex.c', () => {
      const sourceCode = 'int main(void) {\n    return 2 + 3 * 4 - ~5;\n}';
      const assembly = compileAndTest(sourceCode, 20, 'compare_complex'); // 2 + 12 - (-6) = 20
      
      // Should contain all necessary operations
      expect(assembly).toContain('imull'); // multiplication
      expect(assembly).toContain('addl');  // addition
      expect(assembly).toContain('subl');  // subtraction
      expect(assembly).toContain('notl');  // complement
    });

    it('should handle division like test_division.c', () => {
      const sourceCode = 'int main(void) {\n    return 15 / 3 + 17 % 5;\n}';
      const assembly = compileAndTest(sourceCode, 7, 'compare_division');
      
      // Should use idiv for both division and modulo
      const idivCount = (assembly.match(/idivl/g) || []).length;
      expect(idivCount).toBe(2);
      
      // Should use cdq for sign extension
      const cdqCount = (assembly.match(/cdq/g) || []).length;
      expect(cdqCount).toBe(2);
    });
  });

  describe('comprehensive exit code validation', () => {
    const testCases = [
      { code: 'int main(void) { return 0; }', expected: 0, name: 'zero' },
      { code: 'int main(void) { return 1; }', expected: 1, name: 'one' },
      { code: 'int main(void) { return 42; }', expected: 42, name: 'forty_two' },
      { code: 'int main(void) { return 100; }', expected: 100, name: 'hundred' },
      { code: 'int main(void) { return 255; }', expected: 255, name: 'max_byte' },
      { code: 'int main(void) { return 1 + 1; }', expected: 2, name: 'simple_add' },
      { code: 'int main(void) { return 5 - 2; }', expected: 3, name: 'simple_sub' },
      { code: 'int main(void) { return 3 * 7; }', expected: 21, name: 'simple_mult' },
      { code: 'int main(void) { return 20 / 4; }', expected: 5, name: 'simple_div' },
      { code: 'int main(void) { return 13 % 4; }', expected: 1, name: 'simple_mod' },
      { code: 'int main(void) { return ~0; }', expected: 255, name: 'complement_zero' },
      { code: 'int main(void) { return -1; }', expected: 255, name: 'negate_one' },
      { code: 'int main(void) { return (2 + 3) * 4; }', expected: 20, name: 'precedence' },
      { code: 'int main(void) { return 2 * (3 + 4); }', expected: 14, name: 'precedence_rev' },
    ];

    testCases.forEach(({ code, expected, name }) => {
      it(`should return exit code ${expected} for ${name}`, () => {
        compileAndTest(code, expected, `exit_code_${name}`);
      });
    });
  });

  describe('assembly syntax validation', () => {
    it('should generate valid AT&T syntax for all supported operations', () => {
      const testCases = [
        'int main(void) { return 42; }',
        'int main(void) { return 2 + 3; }',
        'int main(void) { return 10 - 5; }',
        'int main(void) { return 3 * 4; }',
        'int main(void) { return 8 / 2; }',
        'int main(void) { return 7 % 3; }',
        'int main(void) { return -42; }',
        'int main(void) { return ~42; }',
        'int main(void) { return (2 + 3) * (4 - 1); }'
      ];

      const expectedResults = [42, 5, 5, 12, 4, 1, 214, 213, 15]; // Expected exit codes for each test case
      
      testCases.forEach((sourceCode, index) => {
        const assembly = compileAndTest(sourceCode, expectedResults[index], `syntax_validation_${index}`);
        
        // Verify AT&T syntax compliance
        expect(assembly).toMatch(/\.globl\s+\w+/);
        expect(assembly).toMatch(/\.type\s+\w+,\s*@function/);
        expect(assembly).toMatch(/\w+:/);
        expect(assembly).toMatch(/pushq\s+%rbp/);
        expect(assembly).toMatch(/movq\s+%rsp,\s*%rbp/);
        expect(assembly).toMatch(/movq\s+%rbp,\s*%rsp/);
        expect(assembly).toMatch(/popq\s+%rbp/);
        expect(assembly).toMatch(/ret/);
        expect(assembly).toMatch(/\.section\s+\.note\.GNU-stack/);
        
        // Reset for next iteration
        resetTempCounter();
      });
    });

    it('should use proper operand syntax throughout', () => {
      const sourceCode = 'int main(void) { return (1 + 2) * (3 + 4); }';
      const assembly = compileAndTest(sourceCode, 21, 'operand_syntax');
      
      // Check immediate values use $ prefix
      const immediateMatches = assembly.match(/\$\d+/g);
      expect(immediateMatches).toBeTruthy();
      expect(immediateMatches!.length).toBeGreaterThan(0);
      
      // Check register names use % prefix
      const registerMatches = assembly.match(/%[a-z0-9]+/g);
      expect(registerMatches).toBeTruthy();
      expect(registerMatches!.length).toBeGreaterThan(0);
      
      // Check memory operands use proper format
      const memoryMatches = assembly.match(/-\d+\(%rbp\)/g);
      expect(memoryMatches).toBeTruthy();
      expect(memoryMatches!.length).toBeGreaterThan(0);
    });
  });
});