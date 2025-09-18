import { describe, it, expect, beforeEach } from 'vitest';
import { 
  lexer, 
  TokenStream, 
  parseProgram,
  emitTacky,
  emitCode,
  generateAsm,
  convertTackyInstruction,
  replacePseudoRegisters,
  fixOperands,
  tackyUnopToAsmUnop,
  tackyBinopToAsmBinop,
  tackyValToOperand,
  emitOperand,
  emitInstruction,
  resetTempCounter,
  UnaryOp,
  BinaryOp,
  UnaryOperator,
  BinaryOperator,
  Reg
} from './compiler';

describe('Assembly Code Generation', () => {
  beforeEach(() => {
    resetTempCounter();
  });

  describe('operator conversion', () => {
    it('should convert tacky unary operators to assembly', () => {
      expect(tackyUnopToAsmUnop(UnaryOp.Complement)).toBe(UnaryOperator.Not);
      expect(tackyUnopToAsmUnop(UnaryOp.Negate)).toBe(UnaryOperator.Neg);
    });

    it('should convert tacky binary operators to assembly', () => {
      expect(tackyBinopToAsmBinop(BinaryOp.Add)).toBe(BinaryOperator.Add);
      expect(tackyBinopToAsmBinop(BinaryOp.Subtract)).toBe(BinaryOperator.Sub);
      expect(tackyBinopToAsmBinop(BinaryOp.Multiply)).toBe(BinaryOperator.Mult);
      expect(tackyBinopToAsmBinop(BinaryOp.Divide)).toBeNull();
      expect(tackyBinopToAsmBinop(BinaryOp.Remainder)).toBeNull();
    });
  });

  describe('tackyValToOperand', () => {
    it('should convert constant to immediate operand', () => {
      const val = { type: 'Constant' as const, value: 42 };
      const operand = tackyValToOperand(val);
      expect(operand).toEqual({ type: 'Imm', value: 42 });
    });

    it('should convert variable to pseudo operand', () => {
      const val = { type: 'Var' as const, value: 'tmp.0' };
      const operand = tackyValToOperand(val);
      expect(operand).toEqual({ type: 'Pseudo', value: 'tmp.0' });
    });
  });

  describe('emitOperand', () => {
    it('should emit immediate operand', () => {
      const operand = { type: 'Imm' as const, value: 42 };
      expect(emitOperand(operand)).toBe('$42');
    });

    it('should emit register operands', () => {
      expect(emitOperand({ type: 'Register', value: Reg.AX })).toBe('%eax');
      expect(emitOperand({ type: 'Register', value: Reg.DX })).toBe('%edx');
      expect(emitOperand({ type: 'Register', value: Reg.R10 })).toBe('%r10d');
      expect(emitOperand({ type: 'Register', value: Reg.R11 })).toBe('%r11d');
    });

    it('should emit stack operand', () => {
      const operand = { type: 'Stack' as const, value: 4 };
      expect(emitOperand(operand)).toBe('-4(%rbp)');
    });

    it('should throw error for pseudo operand', () => {
      const operand = { type: 'Pseudo' as const, value: 'tmp.0' };
      expect(() => emitOperand(operand)).toThrow('should have been replaced');
    });
  });

  describe('convertTackyInstruction', () => {
    it('should convert return instruction', () => {
      const instruction = {
        type: 'Return' as const,
        value: { type: 'Constant' as const, value: 42 }
      };
      
      const asmInstructions = convertTackyInstruction(instruction);
      
      expect(asmInstructions).toHaveLength(2);
      expect(asmInstructions[0]).toEqual({
        type: 'Mov',
        value: {
          src: { type: 'Imm', value: 42 },
          dest: { type: 'Register', value: Reg.AX }
        }
      });
      expect(asmInstructions[1]).toEqual({ type: 'Ret' });
    });

    it('should convert unary complement instruction', () => {
      const instruction = {
        type: 'Unary' as const,
        value: {
          op: UnaryOp.Complement,
          src: { type: 'Constant' as const, value: 42 },
          dst: { type: 'Var' as const, value: 'tmp.0' }
        }
      };
      
      const asmInstructions = convertTackyInstruction(instruction);
      
      expect(asmInstructions).toHaveLength(2);
      expect(asmInstructions[0].type).toBe('Mov');
      expect(asmInstructions[1]).toEqual({
        type: 'Unary',
        value: [UnaryOperator.Not, { type: 'Pseudo', value: 'tmp.0' }]
      });
    });

    it('should convert unary negate instruction', () => {
      const instruction = {
        type: 'Unary' as const,
        value: {
          op: UnaryOp.Negate,
          src: { type: 'Constant' as const, value: 5 },
          dst: { type: 'Var' as const, value: 'tmp.0' }
        }
      };
      
      const asmInstructions = convertTackyInstruction(instruction);
      
      expect(asmInstructions).toHaveLength(2);
      expect(asmInstructions[1]).toEqual({
        type: 'Unary',
        value: [UnaryOperator.Neg, { type: 'Pseudo', value: 'tmp.0' }]
      });
    });

    it('should convert binary add instruction', () => {
      const instruction = {
        type: 'Binary' as const,
        value: {
          op: BinaryOp.Add,
          src1: { type: 'Constant' as const, value: 2 },
          src2: { type: 'Constant' as const, value: 3 },
          dst: { type: 'Var' as const, value: 'tmp.0' }
        }
      };
      
      const asmInstructions = convertTackyInstruction(instruction);
      
      expect(asmInstructions).toHaveLength(2);
      expect(asmInstructions[0].type).toBe('Mov');
      expect(asmInstructions[1]).toEqual({
        type: 'Binary',
        value: [
          BinaryOperator.Add,
          { type: 'Imm', value: 3 },
          { type: 'Pseudo', value: 'tmp.0' }
        ]
      });
    });

    it('should convert binary subtract instruction', () => {
      const instruction = {
        type: 'Binary' as const,
        value: {
          op: BinaryOp.Subtract,
          src1: { type: 'Constant' as const, value: 10 },
          src2: { type: 'Constant' as const, value: 4 },
          dst: { type: 'Var' as const, value: 'tmp.0' }
        }
      };
      
      const asmInstructions = convertTackyInstruction(instruction);
      
      expect(asmInstructions).toHaveLength(2);
      expect(asmInstructions[1].value[0]).toBe(BinaryOperator.Sub);
    });

    it('should convert binary multiply instruction', () => {
      const instruction = {
        type: 'Binary' as const,
        value: {
          op: BinaryOp.Multiply,
          src1: { type: 'Constant' as const, value: 3 },
          src2: { type: 'Constant' as const, value: 4 },
          dst: { type: 'Var' as const, value: 'tmp.0' }
        }
      };
      
      const asmInstructions = convertTackyInstruction(instruction);
      
      expect(asmInstructions).toHaveLength(2);
      expect(asmInstructions[1].value[0]).toBe(BinaryOperator.Mult);
    });

    it('should convert division instruction', () => {
      const instruction = {
        type: 'Binary' as const,
        value: {
          op: BinaryOp.Divide,
          src1: { type: 'Constant' as const, value: 8 },
          src2: { type: 'Constant' as const, value: 2 },
          dst: { type: 'Var' as const, value: 'tmp.0' }
        }
      };
      
      const asmInstructions = convertTackyInstruction(instruction);
      
      expect(asmInstructions).toHaveLength(4);
      expect(asmInstructions[0].type).toBe('Mov'); // Move dividend to AX
      expect(asmInstructions[1].type).toBe('Cdq'); // Sign extend
      expect(asmInstructions[2].type).toBe('Idiv'); // Divide
      expect(asmInstructions[3].type).toBe('Mov'); // Move quotient to destination
    });

    it('should convert remainder instruction', () => {
      const instruction = {
        type: 'Binary' as const,
        value: {
          op: BinaryOp.Remainder,
          src1: { type: 'Constant' as const, value: 7 },
          src2: { type: 'Constant' as const, value: 3 },
          dst: { type: 'Var' as const, value: 'tmp.0' }
        }
      };
      
      const asmInstructions = convertTackyInstruction(instruction);
      
      expect(asmInstructions).toHaveLength(4);
      expect(asmInstructions[3].value.src).toEqual({ type: 'Register', value: Reg.DX });
    });
  });

  describe('replacePseudoRegisters', () => {
    it('should replace pseudo-registers with stack locations', () => {
      const instructions = [
        {
          type: 'Mov' as const,
          value: {
            src: { type: 'Imm' as const, value: 42 },
            dest: { type: 'Pseudo' as const, value: 'tmp.0' }
          }
        },
        {
          type: 'Unary' as const,
          value: [UnaryOperator.Neg, { type: 'Pseudo' as const, value: 'tmp.0' }] as const
        }
      ];
      
      const [newInstructions, stackSize] = replacePseudoRegisters(instructions);
      
      expect(stackSize).toBe(4);
      expect(newInstructions[0].value.dest).toEqual({ type: 'Stack', value: 4 });
      expect(newInstructions[1].value[1]).toEqual({ type: 'Stack', value: 4 });
    });

    it('should assign different stack locations to different pseudo-registers', () => {
      const instructions = [
        {
          type: 'Mov' as const,
          value: {
            src: { type: 'Pseudo' as const, value: 'tmp.0' },
            dest: { type: 'Pseudo' as const, value: 'tmp.1' }
          }
        }
      ];
      
      const [newInstructions, stackSize] = replacePseudoRegisters(instructions);
      
      expect(stackSize).toBe(8);
      expect(newInstructions[0].value.src).toEqual({ type: 'Stack', value: 4 });
      expect(newInstructions[0].value.dest).toEqual({ type: 'Stack', value: 8 });
    });

    it('should reuse stack locations for same pseudo-register', () => {
      const instructions = [
        {
          type: 'Mov' as const,
          value: {
            src: { type: 'Imm' as const, value: 1 },
            dest: { type: 'Pseudo' as const, value: 'tmp.0' }
          }
        },
        {
          type: 'Mov' as const,
          value: {
            src: { type: 'Pseudo' as const, value: 'tmp.0' },
            dest: { type: 'Register' as const, value: Reg.AX }
          }
        }
      ];
      
      const [newInstructions, stackSize] = replacePseudoRegisters(instructions);
      
      expect(stackSize).toBe(4);
      expect(newInstructions[0].value.dest).toEqual({ type: 'Stack', value: 4 });
      expect(newInstructions[1].value.src).toEqual({ type: 'Stack', value: 4 });
    });
  });

  describe('fixOperands', () => {
    it('should add stack allocation instruction', () => {
      const instructions = [
        {
          type: 'Mov' as const,
          value: {
            src: { type: 'Imm' as const, value: 42 },
            dest: { type: 'Register' as const, value: Reg.AX }
          }
        }
      ];
      
      const fixedInstructions = fixOperands(instructions, 8);
      
      expect(fixedInstructions[0]).toEqual({
        type: 'AllocateStack',
        value: 8
      });
      expect(fixedInstructions[1]).toEqual(instructions[0]);
    });

    it('should not add stack allocation for zero stack size', () => {
      const instructions = [
        {
          type: 'Mov' as const,
          value: {
            src: { type: 'Imm' as const, value: 42 },
            dest: { type: 'Register' as const, value: Reg.AX }
          }
        }
      ];
      
      const fixedInstructions = fixOperands(instructions, 0);
      
      expect(fixedInstructions).toHaveLength(1);
      expect(fixedInstructions[0]).toEqual(instructions[0]);
    });

    it('should fix double-stack operands in mov instruction', () => {
      const instructions = [
        {
          type: 'Mov' as const,
          value: {
            src: { type: 'Stack' as const, value: 4 },
            dest: { type: 'Stack' as const, value: 8 }
          }
        }
      ];
      
      const fixedInstructions = fixOperands(instructions, 8);
      
      expect(fixedInstructions).toHaveLength(3); // AllocateStack + 2 Mov instructions
      expect(fixedInstructions[1].type).toBe('Mov');
      expect(fixedInstructions[2].type).toBe('Mov');
    });

    it('should fix double-stack operands in binary instruction', () => {
      const instructions = [
        {
          type: 'Binary' as const,
          value: [
            BinaryOperator.Add,
            { type: 'Stack' as const, value: 4 },
            { type: 'Stack' as const, value: 8 }
          ] as const
        }
      ];
      
      const fixedInstructions = fixOperands(instructions, 8);
      
      expect(fixedInstructions).toHaveLength(3); // AllocateStack + Mov + Binary
      expect(fixedInstructions[1].type).toBe('Mov');
      expect(fixedInstructions[2].type).toBe('Binary');
    });
  });

  describe('emitInstruction', () => {
    it('should emit mov instruction', () => {
      const instruction = {
        type: 'Mov' as const,
        value: {
          src: { type: 'Imm' as const, value: 42 },
          dest: { type: 'Register' as const, value: Reg.AX }
        }
      };
      
      const lines = emitInstruction(instruction);
      
      expect(lines).toEqual(['    movl $42, %eax']);
    });

    it('should emit unary neg instruction', () => {
      const instruction = {
        type: 'Unary' as const,
        value: [UnaryOperator.Neg, { type: 'Register' as const, value: Reg.AX }] as const
      };
      
      const lines = emitInstruction(instruction);
      
      expect(lines).toEqual(['    negl %eax']);
    });

    it('should emit unary not instruction', () => {
      const instruction = {
        type: 'Unary' as const,
        value: [UnaryOperator.Not, { type: 'Register' as const, value: Reg.AX }] as const
      };
      
      const lines = emitInstruction(instruction);
      
      expect(lines).toEqual(['    notl %eax']);
    });

    it('should emit binary add instruction', () => {
      const instruction = {
        type: 'Binary' as const,
        value: [
          BinaryOperator.Add,
          { type: 'Imm' as const, value: 5 },
          { type: 'Register' as const, value: Reg.AX }
        ] as const
      };
      
      const lines = emitInstruction(instruction);
      
      expect(lines).toEqual(['    addl $5, %eax']);
    });

    it('should emit binary sub instruction', () => {
      const instruction = {
        type: 'Binary' as const,
        value: [
          BinaryOperator.Sub,
          { type: 'Imm' as const, value: 3 },
          { type: 'Register' as const, value: Reg.AX }
        ] as const
      };
      
      const lines = emitInstruction(instruction);
      
      expect(lines).toEqual(['    subl $3, %eax']);
    });

    it('should emit binary mult instruction', () => {
      const instruction = {
        type: 'Binary' as const,
        value: [
          BinaryOperator.Mult,
          { type: 'Imm' as const, value: 4 },
          { type: 'Register' as const, value: Reg.AX }
        ] as const
      };
      
      const lines = emitInstruction(instruction);
      
      expect(lines).toEqual(['    imull $4, %eax']);
    });

    it('should handle special case for mult with immediate and memory', () => {
      const instruction = {
        type: 'Binary' as const,
        value: [
          BinaryOperator.Mult,
          { type: 'Imm' as const, value: 4 },
          { type: 'Stack' as const, value: 8 }
        ] as const
      };
      
      const lines = emitInstruction(instruction);
      
      expect(lines).toHaveLength(3);
      expect(lines[0]).toContain('movl');
      expect(lines[1]).toContain('imull');
      expect(lines[2]).toContain('movl');
    });

    it('should emit idiv instruction', () => {
      const instruction = {
        type: 'Idiv' as const,
        value: { type: 'Register' as const, value: Reg.R10 }
      };
      
      const lines = emitInstruction(instruction);
      
      expect(lines).toEqual(['    idivl %r10d']);
    });

    it('should handle idiv with immediate operand', () => {
      const instruction = {
        type: 'Idiv' as const,
        value: { type: 'Imm' as const, value: 2 }
      };
      
      const lines = emitInstruction(instruction);
      
      expect(lines).toHaveLength(2);
      expect(lines[0]).toContain('movl $2');
      expect(lines[1]).toContain('idivl');
    });

    it('should emit cdq instruction', () => {
      const instruction = { type: 'Cdq' as const };
      
      const lines = emitInstruction(instruction);
      
      expect(lines).toEqual(['    cdq']);
    });

    it('should emit stack allocation instruction', () => {
      const instruction = {
        type: 'AllocateStack' as const,
        value: 12
      };
      
      const lines = emitInstruction(instruction);
      
      expect(lines).toEqual(['    subq $16, %rsp']); // Aligned to 16 bytes
    });

    it('should emit ret instruction', () => {
      const instruction = { type: 'Ret' as const };
      
      const lines = emitInstruction(instruction);
      
      expect(lines).toEqual([
        '    movq %rbp, %rsp',
        '    popq %rbp',
        '    ret'
      ]);
    });
  });

  describe('generateAsm', () => {
    it('should generate complete assembly for simple function', () => {
      const code = {
        type: 'ProgramCode' as const,
        value: {
          name: 'main',
          instructions: [
            {
              type: 'Mov' as const,
              value: {
                src: { type: 'Imm' as const, value: 42 },
                dest: { type: 'Register' as const, value: Reg.AX }
              }
            },
            { type: 'Ret' as const }
          ]
        }
      };
      
      const assembly = generateAsm(code);
      
      expect(assembly).toContain('    .globl main');
      expect(assembly).toContain('    .type main, @function');
      expect(assembly).toContain('main:');
      expect(assembly).toContain('    pushq %rbp');
      expect(assembly).toContain('    movq %rsp, %rbp');
      expect(assembly).toContain('    movl $42, %eax');
      expect(assembly).toContain('    movq %rbp, %rsp');
      expect(assembly).toContain('    popq %rbp');
      expect(assembly).toContain('    ret');
      expect(assembly).toContain('.section .note.GNU-stack,"",@progbits');
    });
  });

  describe('integration tests', () => {
    it('should generate correct assembly for simple program', () => {
      const sourceCode = 'int main(void) { return 42; }';
      const tokens = new TokenStream(lexer(sourceCode));
      const ast = parseProgram(tokens);
      const tackyProgram = emitTacky(ast);
      
      const assemblyCode = emitCode(tackyProgram);
      
      expect(assemblyCode.type).toBe('ProgramCode');
      expect(assemblyCode.value.name).toBe('main');
      expect(assemblyCode.value.instructions).toHaveLength(2);
      expect(assemblyCode.value.instructions[0].type).toBe('Mov');
      expect(assemblyCode.value.instructions[1].type).toBe('Ret');
    });

    it('should generate correct assembly for arithmetic expression', () => {
      const sourceCode = 'int main(void) { return 2 + 3; }';
      const tokens = new TokenStream(lexer(sourceCode));
      const ast = parseProgram(tokens);
      const tackyProgram = emitTacky(ast);
      
      const assemblyCode = emitCode(tackyProgram);
      
      expect(assemblyCode.value.instructions.length).toBeGreaterThan(2);
      
      // Should have stack allocation, mov, binary, mov to AX, ret
      const instructions = assemblyCode.value.instructions;
      expect(instructions[0].type).toBe('AllocateStack');
      expect(instructions.some(i => i.type === 'Binary')).toBe(true);
      expect(instructions[instructions.length - 1].type).toBe('Ret');
    });

    it('should generate correct assembly for unary expression', () => {
      const sourceCode = 'int main(void) { return -42; }';
      const tokens = new TokenStream(lexer(sourceCode));
      const ast = parseProgram(tokens);
      const tackyProgram = emitTacky(ast);
      
      const assemblyCode = emitCode(tackyProgram);
      
      const instructions = assemblyCode.value.instructions;
      expect(instructions.some(i => i.type === 'Unary')).toBe(true);
    });

    it('should generate correct assembly for division', () => {
      const sourceCode = 'int main(void) { return 8 / 2; }';
      const tokens = new TokenStream(lexer(sourceCode));
      const ast = parseProgram(tokens);
      const tackyProgram = emitTacky(ast);
      
      const assemblyCode = emitCode(tackyProgram);
      
      const instructions = assemblyCode.value.instructions;
      expect(instructions.some(i => i.type === 'Cdq')).toBe(true);
      expect(instructions.some(i => i.type === 'Idiv')).toBe(true);
    });

    it('should generate correct assembly for modulo', () => {
      const sourceCode = 'int main(void) { return 7 % 3; }';
      const tokens = new TokenStream(lexer(sourceCode));
      const ast = parseProgram(tokens);
      const tackyProgram = emitTacky(ast);
      
      const assemblyCode = emitCode(tackyProgram);
      
      const instructions = assemblyCode.value.instructions;
      expect(instructions.some(i => i.type === 'Cdq')).toBe(true);
      expect(instructions.some(i => i.type === 'Idiv')).toBe(true);
    });

    it('should generate complete assembly text', () => {
      const sourceCode = 'int main(void) { return 42; }';
      const tokens = new TokenStream(lexer(sourceCode));
      const ast = parseProgram(tokens);
      const tackyProgram = emitTacky(ast);
      const assemblyCode = emitCode(tackyProgram);
      
      const assemblyText = generateAsm(assemblyCode);
      
      expect(typeof assemblyText).toBe('string');
      expect(assemblyText).toContain('.globl main');
      expect(assemblyText).toContain('main:');
      expect(assemblyText).toContain('movl $42, %eax');
      expect(assemblyText).toContain('ret');
      expect(assemblyText).toContain('.section .note.GNU-stack');
    });

    it('should handle complex expression with proper stack management', () => {
      const sourceCode = 'int main(void) { return (2 + 3) * (4 - 1); }';
      const tokens = new TokenStream(lexer(sourceCode));
      const ast = parseProgram(tokens);
      const tackyProgram = emitTacky(ast);
      
      const assemblyCode = emitCode(tackyProgram);
      
      const instructions = assemblyCode.value.instructions;
      expect(instructions[0].type).toBe('AllocateStack');
      expect(instructions[0].value).toBeGreaterThan(0);
      
      // Should have multiple binary operations
      const binaryCount = instructions.filter(i => i.type === 'Binary').length;
      expect(binaryCount).toBe(3); // 2+3, 4-1, tmp*tmp
    });
  });
});