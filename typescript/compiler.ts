#!/usr/bin/env node

// C Compiler in TypeScript
// A minimal C compiler that generates x86-64 assembly

// ============================================================================
// TOKEN TYPES AND LEXER DEFINITIONS
// ============================================================================

enum TokenType {
  KwInt = 'KwInt',
  KwVoid = 'KwVoid',
  KwReturn = 'KwReturn',
  Ident = 'Ident',
  Num = 'Num',
  LParen = 'LParen',
  RParen = 'RParen',
  LBrace = 'LBrace',
  RBrace = 'RBrace',
  Semicolon = 'Semicolon',
  Tilde = 'Tilde',
  Minus = 'Minus',
  Slash = 'Slash',
  Star = 'Star',
  Plus = 'Plus',
  DoubleMinus = 'DoubleMinus',
  Percent = 'Percent'
}

interface Token {
  type: TokenType;
  value?: string | number;
}

// ============================================================================
// AST NODE TYPES
// ============================================================================

enum Type {
  Int = 'Int',
  Void = 'Void'
}

enum Unop {
  Complement = 'Complement',
  Negative = 'Negative'
}

enum Binop {
  Subtract = 'Subtract',
  Add = 'Add',
  Multiply = 'Multiply',
  Divide = 'Divide',
  Modulo = 'Modulo'
}

interface UnopExpr {
  unop: Unop;
  expr: Factor;
}

interface BinaryExpr {
  op: Binop;
  left: Expr;
  right: Expr;
}

type Factor = 
  | { type: 'Int'; value: number }
  | { type: 'UnopExpr'; value: UnopExpr }
  | { type: 'Expr'; value: Expr };

type Expr = 
  | { type: 'Factor'; value: Factor }
  | { type: 'Binary'; value: BinaryExpr };

interface Stmt {
  type: 'Return';
  expr?: Expr; // Optional for void returns
}

interface Function {
  retType: Type;
  name: string;
  params: Array<[Type, string]>;
  body: Stmt[];
}

interface Program {
  funcs: Function[];
}

// ============================================================================
// TACKY INTERMEDIATE REPRESENTATION
// ============================================================================

enum UnaryOp {
  Complement = 'Complement',
  Negate = 'Negate'
}

enum BinaryOp {
  Add = 'Add',
  Subtract = 'Subtract',
  Multiply = 'Multiply',
  Divide = 'Divide',
  Remainder = 'Remainder'
}

type Val = 
  | { type: 'Constant'; value: number }
  | { type: 'Var'; value: string };

interface UnaryInstruction {
  op: UnaryOp;
  src: Val;
  dst: Val;
}

interface BinaryInstruction {
  op: BinaryOp;
  src1: Val;
  src2: Val;
  dst: Val;
}

type Instruction = 
  | { type: 'Return'; value: Val }
  | { type: 'Unary'; value: UnaryInstruction }
  | { type: 'Binary'; value: BinaryInstruction };

interface FunctionDefinition {
  identifier: string;
  body: Instruction[];
}

interface TackyProgram {
  type: 'Program';
  value: FunctionDefinition;
}

// ============================================================================
// ASSEMBLY CODE REPRESENTATION
// ============================================================================

enum Reg {
  AX = 'AX',
  DX = 'DX',
  R10 = 'R10',
  R11 = 'R11'
}

type Operand = 
  | { type: 'Imm'; value: number }
  | { type: 'Register'; value: Reg }
  | { type: 'Pseudo'; value: string }
  | { type: 'Stack'; value: number };

enum UnaryOperator {
  Neg = 'Neg',
  Not = 'Not'
}

enum BinaryOperator {
  Add = 'Add',
  Sub = 'Sub',
  Mult = 'Mult'
}

interface Mov {
  src: Operand;
  dest: Operand;
}

type InstructionCode = 
  | { type: 'Mov'; value: Mov }
  | { type: 'Unary'; value: [UnaryOperator, Operand] }
  | { type: 'AllocateStack'; value: number }
  | { type: 'Ret' }
  | { type: 'Binary'; value: [BinaryOperator, Operand, Operand] }
  | { type: 'Idiv'; value: Operand }
  | { type: 'Cdq' };

interface FunctionCode {
  name: string;
  instructions: InstructionCode[];
}

interface Code {
  type: 'ProgramCode';
  value: FunctionCode;
}

// ============================================================================
// LEXICAL ANALYSIS (TOKENIZER)
// ============================================================================

class LexerError extends Error {
  constructor(message: string, position: number) {
    super(`Lexer error at position ${position}: ${message}`);
    this.name = 'LexerError';
  }
}

function isWhitespace(char: string): boolean {
  return /\s/.test(char);
}

function isAlpha(char: string): boolean {
  return /[a-zA-Z_]/.test(char);
}

function isAlphaNumeric(char: string): boolean {
  return /[a-zA-Z0-9_]/.test(char);
}

function isDigit(char: string): boolean {
  return /[0-9]/.test(char);
}

function tokenizeKeywordOrIdent(input: string, start: number): [Token, number] {
  let end = start;
  
  // First character must be alpha or underscore
  if (!isAlpha(input[start])) {
    throw new LexerError(`Invalid identifier start character: '${input[start]}'`, start);
  }
  
  // Continue while alphanumeric or underscore
  while (end < input.length && isAlphaNumeric(input[end])) {
    end++;
  }
  
  const text = input.substring(start, end);
  
  // Check for keywords
  switch (text) {
    case 'int':
      return [{ type: TokenType.KwInt }, end];
    case 'void':
      return [{ type: TokenType.KwVoid }, end];
    case 'return':
      return [{ type: TokenType.KwReturn }, end];
    default:
      return [{ type: TokenType.Ident, value: text }, end];
  }
}

function tokenizeNumber(input: string, start: number): [Token, number] {
  let end = start;
  
  // Continue while digits
  while (end < input.length && isDigit(input[end])) {
    end++;
  }
  
  const text = input.substring(start, end);
  const value = parseInt(text, 10);
  
  // Check for integer overflow or invalid number
  if (isNaN(value)) {
    throw new LexerError(`Invalid number: '${text}'`, start);
  }
  
  return [{ type: TokenType.Num, value }, end];
}

function tokenizeOperator(input: string, start: number): [Token, number] {
  const char = input[start];
  
  // Check for double-minus first (two-character operator)
  if (char === '-' && start + 1 < input.length && input[start + 1] === '-') {
    return [{ type: TokenType.DoubleMinus }, start + 2];
  }
  
  // Single-character operators
  switch (char) {
    case '(':
      return [{ type: TokenType.LParen }, start + 1];
    case ')':
      return [{ type: TokenType.RParen }, start + 1];
    case '{':
      return [{ type: TokenType.LBrace }, start + 1];
    case '}':
      return [{ type: TokenType.RBrace }, start + 1];
    case ';':
      return [{ type: TokenType.Semicolon }, start + 1];
    case '~':
      return [{ type: TokenType.Tilde }, start + 1];
    case '-':
      return [{ type: TokenType.Minus }, start + 1];
    case '/':
      return [{ type: TokenType.Slash }, start + 1];
    case '*':
      return [{ type: TokenType.Star }, start + 1];
    case '+':
      return [{ type: TokenType.Plus }, start + 1];
    case '%':
      return [{ type: TokenType.Percent }, start + 1];
    default:
      throw new LexerError(`Unrecognized character: '${char}'`, start);
  }
}

function lexer(input: string): Token[] {
  const tokens: Token[] = [];
  let position = 0;
  
  while (position < input.length) {
    const char = input[position];
    
    // Skip whitespace
    if (isWhitespace(char)) {
      position++;
      continue;
    }
    
    try {
      // Try to tokenize based on character type
      if (isAlpha(char)) {
        // Keyword or identifier
        const [token, newPos] = tokenizeKeywordOrIdent(input, position);
        tokens.push(token);
        position = newPos;
      } else if (isDigit(char)) {
        // Number
        const [token, newPos] = tokenizeNumber(input, position);
        tokens.push(token);
        position = newPos;
      } else {
        // Operator or punctuation
        const [token, newPos] = tokenizeOperator(input, position);
        tokens.push(token);
        position = newPos;
      }
    } catch (error) {
      if (error instanceof LexerError) {
        console.error(error.message);
        process.exit(1);
      }
      throw error;
    }
  }
  
  return tokens;
}

// ============================================================================
// UTILITY CLASSES
// ============================================================================

class TokenStream {
  private queue: Token[];
  private position: number;

  constructor(tokens: Token[]) {
    this.queue = tokens;
    this.position = 0;
  }

  takeToken(): Token | null {
    if (this.position >= this.queue.length) {
      return null;
    }
    return this.queue[this.position++];
  }

  peek(): Token | null {
    if (this.position >= this.queue.length) {
      return null;
    }
    return this.queue[this.position];
  }

  npeek(n: number): Token[] {
    const result: Token[] = [];
    for (let i = 0; i < n && this.position + i < this.queue.length; i++) {
      result.push(this.queue[this.position + i]);
    }
    return result;
  }

  isEmpty(): boolean {
    return this.position >= this.queue.length;
  }
}

// ============================================================================
// PARSER IMPLEMENTATION
// ============================================================================

class ParseError extends Error {
  constructor(message: string, token?: Token | null) {
    super(token ? `Parse error: ${message} (got ${token.type})` : `Parse error: ${message}`);
    this.name = 'ParseError';
  }
}

// Operator precedence table
const PRECEDENCE: Partial<Record<TokenType, number>> = {
  [TokenType.Star]: 50,      // Multiplication
  [TokenType.Slash]: 50,     // Division
  [TokenType.Percent]: 50,   // Modulo
  [TokenType.Plus]: 45,      // Addition
  [TokenType.Minus]: 45,     // Subtraction
};

function getTokenPrecedence(tokenType: TokenType): number {
  return PRECEDENCE[tokenType] || 0;
}

function tokenToBinop(tokenType: TokenType): Binop {
  switch (tokenType) {
    case TokenType.Plus:
      return Binop.Add;
    case TokenType.Minus:
      return Binop.Subtract;
    case TokenType.Star:
      return Binop.Multiply;
    case TokenType.Slash:
      return Binop.Divide;
    case TokenType.Percent:
      return Binop.Modulo;
    default:
      throw new ParseError(`Invalid binary operator: ${tokenType}`);
  }
}

function tokenToUnop(tokenType: TokenType): Unop {
  switch (tokenType) {
    case TokenType.Tilde:
      return Unop.Complement;
    case TokenType.Minus:
      return Unop.Negative;
    default:
      throw new ParseError(`Invalid unary operator: ${tokenType}`);
  }
}

function parsePrimaryExp(tokens: TokenStream): Factor {
  const token = tokens.peek();
  
  if (!token) {
    throw new ParseError('Unexpected end of input, expected expression');
  }

  // Handle numbers
  if (token.type === TokenType.Num) {
    tokens.takeToken(); // consume the number token
    if (typeof token.value !== 'number') {
      throw new ParseError('Invalid number token');
    }
    return { type: 'Int', value: token.value };
  }

  // Handle parenthesized expressions
  if (token.type === TokenType.LParen) {
    tokens.takeToken(); // consume '('
    const expr = parseExp(tokens, 0); // Parse inner expression with minimum precedence
    
    const closeParen = tokens.takeToken();
    if (!closeParen || closeParen.type !== TokenType.RParen) {
      throw new ParseError('Expected closing parenthesis', closeParen);
    }
    
    return { type: 'Expr', value: expr };
  }

  // Handle unary operators
  if (token.type === TokenType.Tilde || token.type === TokenType.Minus) {
    tokens.takeToken(); // consume the unary operator
    const unop = tokenToUnop(token.type);
    const expr = parsePrimaryExp(tokens); // Parse the operand
    
    return { type: 'UnopExpr', value: { unop, expr } };
  }

  throw new ParseError(`Unexpected token in primary expression: ${token.type}`, token);
}

function parseExp(tokens: TokenStream, minPrec: number): Expr {
  // Parse the left-hand side (primary expression)
  let left: Expr = { type: 'Factor', value: parsePrimaryExp(tokens) };

  // Precedence climbing algorithm
  while (true) {
    const token = tokens.peek();
    
    // Check if we have a binary operator
    if (!token || !(token.type in PRECEDENCE)) {
      break;
    }

    const prec = getTokenPrecedence(token.type);
    
    // If precedence is too low, stop parsing
    if (prec < minPrec) {
      break;
    }

    // Consume the operator
    tokens.takeToken();
    const op = tokenToBinop(token.type);

    // Parse the right-hand side with higher precedence
    // For left-associative operators, we use prec + 1
    const right = parseExp(tokens, prec + 1);

    // Create binary expression
    left = { type: 'Binary', value: { op, left, right } };
  }

  return left;
}

function parseStatement(tokens: TokenStream): Stmt {
  const token = tokens.peek();
  
  if (!token) {
    throw new ParseError('Unexpected end of input, expected statement');
  }

  // Handle return statements
  if (token.type === TokenType.KwReturn) {
    tokens.takeToken(); // consume 'return'
    
    // Check if this is a void return (return;) or return with expression
    const nextToken = tokens.peek();
    if (nextToken && nextToken.type === TokenType.Semicolon) {
      // Void return - just consume semicolon
      tokens.takeToken();
      return { type: 'Return' }; // No expression for void return
    } else {
      // Return with expression
      const expr = parseExp(tokens, 0);
      
      // Expect semicolon
      const semicolon = tokens.takeToken();
      if (!semicolon || semicolon.type !== TokenType.Semicolon) {
        throw new ParseError('Expected semicolon after return statement', semicolon);
      }
      
      return { type: 'Return', expr };
    }
  }

  throw new ParseError(`Unexpected token in statement: ${token.type}`, token);
}

function parseType(tokens: TokenStream): Type {
  const token = tokens.takeToken();
  
  if (!token) {
    throw new ParseError('Unexpected end of input, expected type');
  }

  switch (token.type) {
    case TokenType.KwInt:
      return Type.Int;
    case TokenType.KwVoid:
      return Type.Void;
    default:
      throw new ParseError(`Expected type (int or void), got ${token.type}`, token);
  }
}

function parseFunction(tokens: TokenStream): Function {
  // Parse return type
  const retType = parseType(tokens);
  
  // Parse function name
  const nameToken = tokens.takeToken();
  if (!nameToken || nameToken.type !== TokenType.Ident) {
    throw new ParseError('Expected function name (identifier)', nameToken);
  }
  
  if (typeof nameToken.value !== 'string') {
    throw new ParseError('Invalid function name token');
  }
  
  const name = nameToken.value;
  
  // Parse parameter list
  const openParen = tokens.takeToken();
  if (!openParen || openParen.type !== TokenType.LParen) {
    throw new ParseError('Expected opening parenthesis after function name', openParen);
  }
  
  // For now, we only support void parameter lists (empty parameters)
  const params: Array<[Type, string]> = [];
  
  // Check if we have void or empty parameter list
  const nextToken = tokens.peek();
  if (nextToken && nextToken.type === TokenType.KwVoid) {
    tokens.takeToken(); // consume 'void'
  }
  // If it's not void, we assume empty parameter list (no parameters)
  
  const closeParen = tokens.takeToken();
  if (!closeParen || closeParen.type !== TokenType.RParen) {
    throw new ParseError('Expected closing parenthesis after parameter list', closeParen);
  }
  
  // Parse function body
  const openBrace = tokens.takeToken();
  if (!openBrace || openBrace.type !== TokenType.LBrace) {
    throw new ParseError('Expected opening brace for function body', openBrace);
  }
  
  // Parse statements in the function body
  const body: Stmt[] = [];
  
  while (true) {
    const token = tokens.peek();
    
    if (!token) {
      throw new ParseError('Unexpected end of input, expected closing brace');
    }
    
    if (token.type === TokenType.RBrace) {
      break; // End of function body
    }
    
    // Parse statement
    const stmt = parseStatement(tokens);
    body.push(stmt);
  }
  
  const closeBrace = tokens.takeToken();
  if (!closeBrace || closeBrace.type !== TokenType.RBrace) {
    throw new ParseError('Expected closing brace after function body', closeBrace);
  }
  
  return { retType, name, params, body };
}

function parseProgram(tokens: TokenStream): Program {
  const funcs: Function[] = [];
  
  while (!tokens.isEmpty()) {
    const func = parseFunction(tokens);
    funcs.push(func);
  }
  
  if (funcs.length === 0) {
    throw new ParseError('Program must contain at least one function');
  }
  
  return { funcs };
}

// ============================================================================
// TACKY CODE GENERATION
// ============================================================================

// Global counter for generating unique temporary variable names
let tempCounter = 0;

function generateTempVar(): string {
  return `tmp.${tempCounter++}`;
}

function resetTempCounter(): void {
  tempCounter = 0;
}

function astUnopToTackyUnop(unop: Unop): UnaryOp {
  switch (unop) {
    case Unop.Complement:
      return UnaryOp.Complement;
    case Unop.Negative:
      return UnaryOp.Negate;
    default:
      throw new Error(`Unknown unary operator: ${unop}`);
  }
}

function astBinopToTackyBinop(binop: Binop): BinaryOp {
  switch (binop) {
    case Binop.Add:
      return BinaryOp.Add;
    case Binop.Subtract:
      return BinaryOp.Subtract;
    case Binop.Multiply:
      return BinaryOp.Multiply;
    case Binop.Divide:
      return BinaryOp.Divide;
    case Binop.Modulo:
      return BinaryOp.Remainder;
    default:
      throw new Error(`Unknown binary operator: ${binop}`);
  }
}

function emitTackyExpr(expr: Expr, instructions: Instruction[]): Val {
  switch (expr.type) {
    case 'Factor':
      return emitTackyFactor(expr.value, instructions);
    
    case 'Binary': {
      const { op, left, right } = expr.value;
      
      // Emit instructions for left operand
      const leftVal = emitTackyExpr(left, instructions);
      
      // Emit instructions for right operand
      const rightVal = emitTackyExpr(right, instructions);
      
      // Generate temporary variable for result
      const resultVar = generateTempVar();
      const resultVal: Val = { type: 'Var', value: resultVar };
      
      // Convert AST binary operator to tacky binary operator
      const tackyOp = astBinopToTackyBinop(op);
      
      // Emit binary instruction
      const binaryInstr: BinaryInstruction = {
        op: tackyOp,
        src1: leftVal,
        src2: rightVal,
        dst: resultVal
      };
      
      instructions.push({ type: 'Binary', value: binaryInstr });
      
      return resultVal;
    }
    
    default:
      throw new Error(`Unknown expression type: ${(expr as any).type}`);
  }
}

function emitTackyFactor(factor: Factor, instructions: Instruction[]): Val {
  switch (factor.type) {
    case 'Int':
      // Integer constants are returned directly as constant values
      return { type: 'Constant', value: factor.value };
    
    case 'UnopExpr': {
      const { unop, expr } = factor.value;
      
      // Emit instructions for the operand
      const operandVal = emitTackyFactor(expr, instructions);
      
      // Generate temporary variable for result
      const resultVar = generateTempVar();
      const resultVal: Val = { type: 'Var', value: resultVar };
      
      // Convert AST unary operator to tacky unary operator
      const tackyOp = astUnopToTackyUnop(unop);
      
      // Emit unary instruction
      const unaryInstr: UnaryInstruction = {
        op: tackyOp,
        src: operandVal,
        dst: resultVal
      };
      
      instructions.push({ type: 'Unary', value: unaryInstr });
      
      return resultVal;
    }
    
    case 'Expr':
      // Recursively handle nested expressions
      return emitTackyExpr(factor.value, instructions);
    
    default:
      throw new Error(`Unknown factor type: ${(factor as any).type}`);
  }
}

function emitTackyStatement(stmt: Stmt, instructions: Instruction[]): void {
  switch (stmt.type) {
    case 'Return':
      if (stmt.expr) {
        // Return with expression
        const returnVal = emitTackyExpr(stmt.expr, instructions);
        instructions.push({ type: 'Return', value: returnVal });
      } else {
        // Void return - return constant 0
        instructions.push({ type: 'Return', value: { type: 'Constant', value: 0 } });
      }
      break;
    
    default:
      throw new Error(`Unknown statement type: ${(stmt as any).type}`);
  }
}

function emitTackyFunction(func: Function): FunctionDefinition {
  const instructions: Instruction[] = [];
  
  // Reset temporary variable counter for each function
  resetTempCounter();
  
  // Emit instructions for each statement in the function body
  for (const stmt of func.body) {
    emitTackyStatement(stmt, instructions);
  }
  
  return {
    identifier: func.name,
    body: instructions
  };
}

function emitTacky(program: Program): TackyProgram {
  // For now, we only support single-function programs
  if (program.funcs.length !== 1) {
    throw new Error('Only single-function programs are supported');
  }
  
  const func = program.funcs[0];
  const tackyFunc = emitTackyFunction(func);
  
  return {
    type: 'Program',
    value: tackyFunc
  };
}

// ============================================================================
// ASSEMBLY CODE GENERATION
// ============================================================================

function tackyUnopToAsmUnop(unop: UnaryOp): UnaryOperator {
  switch (unop) {
    case UnaryOp.Complement:
      return UnaryOperator.Not;
    case UnaryOp.Negate:
      return UnaryOperator.Neg;
    default:
      throw new Error(`Unknown tacky unary operator: ${unop}`);
  }
}

function tackyBinopToAsmBinop(binop: BinaryOp): BinaryOperator | null {
  switch (binop) {
    case BinaryOp.Add:
      return BinaryOperator.Add;
    case BinaryOp.Subtract:
      return BinaryOperator.Sub;
    case BinaryOp.Multiply:
      return BinaryOperator.Mult;
    case BinaryOp.Divide:
    case BinaryOp.Remainder:
      return null; // These use special idiv instruction
    default:
      throw new Error(`Unknown tacky binary operator: ${binop}`);
  }
}

function tackyValToOperand(val: Val): Operand {
  switch (val.type) {
    case 'Constant':
      return { type: 'Imm', value: val.value };
    case 'Var':
      return { type: 'Pseudo', value: val.value };
    default:
      throw new Error(`Unknown tacky value type: ${(val as any).type}`);
  }
}

// Pass 1: Convert tacky instructions to assembly with pseudo-registers
function convertTackyInstruction(instruction: Instruction): InstructionCode[] {
  switch (instruction.type) {
    case 'Return': {
      const src = tackyValToOperand(instruction.value);
      const dest: Operand = { type: 'Register', value: Reg.AX };
      
      return [
        { type: 'Mov', value: { src, dest } },
        { type: 'Ret' }
      ];
    }
    
    case 'Unary': {
      const { op, src, dst } = instruction.value;
      const srcOperand = tackyValToOperand(src);
      const dstOperand = tackyValToOperand(dst);
      const asmOp = tackyUnopToAsmUnop(op);
      
      return [
        { type: 'Mov', value: { src: srcOperand, dest: dstOperand } },
        { type: 'Unary', value: [asmOp, dstOperand] }
      ];
    }
    
    case 'Binary': {
      const { op, src1, src2, dst } = instruction.value;
      const src1Operand = tackyValToOperand(src1);
      const src2Operand = tackyValToOperand(src2);
      const dstOperand = tackyValToOperand(dst);
      
      if (op === BinaryOp.Divide || op === BinaryOp.Remainder) {
        // Division and modulo use special idiv instruction
        const axReg: Operand = { type: 'Register', value: Reg.AX };
        const dxReg: Operand = { type: 'Register', value: Reg.DX };
        
        const instructions: InstructionCode[] = [
          // Move dividend to AX
          { type: 'Mov', value: { src: src1Operand, dest: axReg } },
          // Sign-extend AX to DX:AX
          { type: 'Cdq' },
          // Divide by src2
          { type: 'Idiv', value: src2Operand }
        ];
        
        // Move result to destination
        if (op === BinaryOp.Divide) {
          // Quotient is in AX
          instructions.push({ type: 'Mov', value: { src: axReg, dest: dstOperand } });
        } else {
          // Remainder is in DX
          instructions.push({ type: 'Mov', value: { src: dxReg, dest: dstOperand } });
        }
        
        return instructions;
      } else {
        // Regular binary operations
        const asmOp = tackyBinopToAsmBinop(op);
        if (!asmOp) {
          throw new Error(`Unsupported binary operation: ${op}`);
        }
        
        return [
          { type: 'Mov', value: { src: src1Operand, dest: dstOperand } },
          { type: 'Binary', value: [asmOp, src2Operand, dstOperand] }
        ];
      }
    }
    
    default:
      throw new Error(`Unknown tacky instruction type: ${(instruction as any).type}`);
  }
}

// Pass 2: Replace pseudo-registers with stack locations and track stack usage
function replacePseudoRegisters(instructions: InstructionCode[]): [InstructionCode[], number] {
  const pseudoToStack = new Map<string, number>();
  let stackOffset = 0;
  
  // Helper function to replace operand if it's a pseudo-register
  function replaceOperand(operand: Operand): Operand {
    if (operand.type === 'Pseudo') {
      if (!pseudoToStack.has(operand.value)) {
        stackOffset += 4; // Each stack slot is 4 bytes
        pseudoToStack.set(operand.value, stackOffset);
      }
      return { type: 'Stack', value: pseudoToStack.get(operand.value)! };
    }
    return operand;
  }
  
  const newInstructions: InstructionCode[] = [];
  
  for (const instruction of instructions) {
    switch (instruction.type) {
      case 'Mov': {
        const { src, dest } = instruction.value;
        newInstructions.push({
          type: 'Mov',
          value: {
            src: replaceOperand(src),
            dest: replaceOperand(dest)
          }
        });
        break;
      }
      
      case 'Unary': {
        const [op, operand] = instruction.value;
        newInstructions.push({
          type: 'Unary',
          value: [op, replaceOperand(operand)]
        });
        break;
      }
      
      case 'Binary': {
        const [op, src, dest] = instruction.value;
        newInstructions.push({
          type: 'Binary',
          value: [op, replaceOperand(src), replaceOperand(dest)]
        });
        break;
      }
      
      case 'Idiv': {
        newInstructions.push({
          type: 'Idiv',
          value: replaceOperand(instruction.value)
        });
        break;
      }
      
      case 'Ret':
      case 'Cdq':
      case 'AllocateStack':
        // These instructions don't have operands to replace
        newInstructions.push(instruction);
        break;
      
      default:
        throw new Error(`Unknown instruction type in pass 2: ${(instruction as any).type}`);
    }
  }
  
  return [newInstructions, stackOffset];
}

// Pass 3: Fix double-stack operand issues and add stack allocation
function fixOperands(instructions: InstructionCode[], stackSize: number): InstructionCode[] {
  const newInstructions: InstructionCode[] = [];
  
  // Add stack allocation at the beginning if needed
  if (stackSize > 0) {
    newInstructions.push({ type: 'AllocateStack', value: stackSize });
  }
  
  for (const instruction of instructions) {
    switch (instruction.type) {
      case 'Mov': {
        const { src, dest } = instruction.value;
        
        // If both operands are stack locations, we need to use a temporary register
        if (src.type === 'Stack' && dest.type === 'Stack') {
          const tempReg: Operand = { type: 'Register', value: Reg.R10 };
          newInstructions.push(
            { type: 'Mov', value: { src, dest: tempReg } },
            { type: 'Mov', value: { src: tempReg, dest } }
          );
        } else {
          newInstructions.push(instruction);
        }
        break;
      }
      
      case 'Binary': {
        const [op, src, dest] = instruction.value;
        
        // If both operands are stack locations, we need to use a temporary register
        if (src.type === 'Stack' && dest.type === 'Stack') {
          const tempReg: Operand = { type: 'Register', value: Reg.R11 };
          newInstructions.push(
            { type: 'Mov', value: { src, dest: tempReg } },
            { type: 'Binary', value: [op, tempReg, dest] }
          );
        } else {
          newInstructions.push(instruction);
        }
        break;
      }
      
      default:
        // Other instructions don't have double-operand issues
        newInstructions.push(instruction);
        break;
    }
  }
  
  return newInstructions;
}

// Main assembly code generation function
function emitCode(tackyProgram: TackyProgram): Code {
  const tackyFunc = tackyProgram.value;
  
  // Pass 1: Convert tacky instructions to assembly with pseudo-registers
  let instructions: InstructionCode[] = [];
  for (const tackyInstruction of tackyFunc.body) {
    const asmInstructions = convertTackyInstruction(tackyInstruction);
    instructions.push(...asmInstructions);
  }
  
  // Pass 2: Replace pseudo-registers with stack locations
  const [instructionsWithStack, stackSize] = replacePseudoRegisters(instructions);
  
  // Pass 3: Fix operand issues and add stack allocation
  const finalInstructions = fixOperands(instructionsWithStack, stackSize);
  
  const functionCode: FunctionCode = {
    name: tackyFunc.identifier,
    instructions: finalInstructions
  };
  
  return {
    type: 'ProgramCode',
    value: functionCode
  };
}

// ============================================================================
// ASSEMBLY OUTPUT GENERATION
// ============================================================================

function emitOperand(operand: Operand): string {
  switch (operand.type) {
    case 'Imm':
      return `$${operand.value}`;
    case 'Register':
      switch (operand.value) {
        case Reg.AX:
          return '%eax';
        case Reg.DX:
          return '%edx';
        case Reg.R10:
          return '%r10d';
        case Reg.R11:
          return '%r11d';
        default:
          throw new Error(`Unknown register: ${operand.value}`);
      }
    case 'Stack':
      return `-${operand.value}(%rbp)`;
    case 'Pseudo':
      throw new Error(`Pseudo-register ${operand.value} should have been replaced`);
    default:
      throw new Error(`Unknown operand type: ${(operand as any).type}`);
  }
}

function emitInstruction(instruction: InstructionCode): string[] {
  switch (instruction.type) {
    case 'Mov': {
      const { src, dest } = instruction.value;
      return [`    movl ${emitOperand(src)}, ${emitOperand(dest)}`];
    }
    
    case 'Unary': {
      const [op, operand] = instruction.value;
      const opStr = op === UnaryOperator.Neg ? 'negl' : 'notl';
      return [`    ${opStr} ${emitOperand(operand)}`];
    }
    
    case 'Binary': {
      const [op, src, dest] = instruction.value;
      
      // Special handling for multiplication
      if (op === BinaryOperator.Mult) {
        // imull with two operands requires destination to be a register
        if (dest.type === 'Stack') {
          const tempReg: Operand = { type: 'Register', value: Reg.R10 };
          return [
            `    movl ${emitOperand(dest)}, ${emitOperand(tempReg)}`,
            `    imull ${emitOperand(src)}, ${emitOperand(tempReg)}`,
            `    movl ${emitOperand(tempReg)}, ${emitOperand(dest)}`
          ];
        } else if (src.type === 'Imm' && dest.type === 'Stack') {
          // This case is already handled above, but keeping for clarity
          const tempReg: Operand = { type: 'Register', value: Reg.R10 };
          return [
            `    movl ${emitOperand(dest)}, ${emitOperand(tempReg)}`,
            `    imull ${emitOperand(src)}, ${emitOperand(tempReg)}`,
            `    movl ${emitOperand(tempReg)}, ${emitOperand(dest)}`
          ];
        }
      }
      
      let opStr: string;
      switch (op) {
        case BinaryOperator.Add:
          opStr = 'addl';
          break;
        case BinaryOperator.Sub:
          opStr = 'subl';
          break;
        case BinaryOperator.Mult:
          opStr = 'imull';
          break;
        default:
          throw new Error(`Unknown binary operator: ${op}`);
      }
      return [`    ${opStr} ${emitOperand(src)}, ${emitOperand(dest)}`];
    }
    
    case 'Idiv': {
      const operand = instruction.value;
      // idiv cannot use immediate operands, must use register or memory
      if (operand.type === 'Imm') {
        // Load immediate into temporary register first
        const tempReg: Operand = { type: 'Register', value: Reg.R11 };
        return [
          `    movl ${emitOperand(operand)}, ${emitOperand(tempReg)}`,
          `    idivl ${emitOperand(tempReg)}`
        ];
      } else {
        return [`    idivl ${emitOperand(operand)}`];
      }
    }
    
    case 'Cdq': {
      return ['    cdq'];
    }
    
    case 'AllocateStack': {
      const stackSize = instruction.value;
      // Align stack to 16-byte boundary
      const alignedSize = Math.ceil(stackSize / 16) * 16;
      return [`    subq $${alignedSize}, %rsp`];
    }
    
    case 'Ret': {
      return [
        '    movq %rbp, %rsp',
        '    popq %rbp',
        '    ret'
      ];
    }
    
    default:
      throw new Error(`Unknown instruction type: ${(instruction as any).type}`);
  }
}

function generateAsm(code: Code): string {
  const func = code.value;
  const lines: string[] = [];
  
  // Function declaration
  lines.push('    .globl ' + func.name);
  lines.push('    .type ' + func.name + ', @function');
  lines.push(func.name + ':');
  
  // Function prologue
  lines.push('    pushq %rbp');
  lines.push('    movq %rsp, %rbp');
  
  // Emit instructions
  for (const instruction of func.instructions) {
    const instructionLines = emitInstruction(instruction);
    lines.push(...instructionLines);
  }
  
  // Add GNU stack note section for proper linking at the end
  lines.push('');
  lines.push('.section .note.GNU-stack,"",@progbits');
  
  return lines.join('\n') + '\n';
}

// ============================================================================
// DEBUG OUTPUT FUNCTIONS
// ============================================================================

function printAST(program: Program): void {
  console.log(formatAST(program));
}

function formatAST(program: Program): string {
  return `Program { funcs: [${program.funcs.map(formatFunction).join(', ')}] }`;
}

function formatFunction(func: Function): string {
  const params = func.params.map(([type, name]) => `(${formatType(type)}, "${name}")`).join(', ');
  const body = func.body.map(formatStatement).join(', ');
  return `Function { ret_type: ${formatType(func.retType)}, name: "${func.name}", params: [${params}], body: [${body}] }`;
}

function formatType(type: Type): string {
  return type === Type.Int ? 'Int' : 'Void';
}

function formatStatement(stmt: Stmt): string {
  if (stmt.expr) {
    return `Return(${formatExpr(stmt.expr)})`;
  } else {
    return 'Return(Factor(Int(0)))'; // Void return represented as return 0
  }
}

function formatExpr(expr: Expr): string {
  switch (expr.type) {
    case 'Factor':
      return `Factor(${formatFactor(expr.value)})`;
    case 'Binary':
      return `Binary(BinaryExpr { op: ${formatBinop(expr.value.op)}, left: ${formatExpr(expr.value.left)}, right: ${formatExpr(expr.value.right)} })`;
    default:
      return 'Unknown';
  }
}

function formatFactor(factor: Factor): string {
  switch (factor.type) {
    case 'Int':
      return `Int(${factor.value})`;
    case 'UnopExpr':
      return `UnopExpr(UnopExpr { unop: ${formatUnop(factor.value.unop)}, expr: ${formatFactor(factor.value.expr)} })`;
    case 'Expr':
      return `Expr(${formatExpr(factor.value)})`;
    default:
      return 'Unknown';
  }
}

function formatUnop(unop: Unop): string {
  switch (unop) {
    case Unop.Complement:
      return 'Complement';
    case Unop.Negative:
      return 'Negative';
    default:
      return 'Unknown';
  }
}

function formatBinop(binop: Binop): string {
  switch (binop) {
    case Binop.Add:
      return 'Add';
    case Binop.Subtract:
      return 'Subtract';
    case Binop.Multiply:
      return 'Multiply';
    case Binop.Divide:
      return 'Divide';
    case Binop.Modulo:
      return 'Modulo';
    default:
      return 'Unknown';
  }
}

function printTackyIR(tackyProgram: TackyProgram): void {
  console.log(formatTackyProgram(tackyProgram));
}

function formatTackyProgram(tackyProgram: TackyProgram): string {
  return `Program(${formatTackyFunction(tackyProgram.value)})`;
}

function formatTackyFunction(func: FunctionDefinition): string {
  const instructions = func.body.map(formatTackyInstruction).join(', ');
  return `FunctionDefinition { identifier: "${func.identifier}", body: [${instructions}] }`;
}

function formatTackyInstruction(instruction: Instruction): string {
  switch (instruction.type) {
    case 'Return':
      return `Return(${formatTackyVal(instruction.value)})`;
    case 'Unary':
      return `Unary(UnaryInstruction { op: ${formatTackyUnaryOp(instruction.value.op)}, src: ${formatTackyVal(instruction.value.src)}, dst: ${formatTackyVal(instruction.value.dst)} })`;
    case 'Binary':
      return `Binary(BinaryInstruction { op: ${formatTackyBinaryOp(instruction.value.op)}, src1: ${formatTackyVal(instruction.value.src1)}, src2: ${formatTackyVal(instruction.value.src2)}, dst: ${formatTackyVal(instruction.value.dst)} })`;
    default:
      return 'Unknown';
  }
}

function formatTackyVal(val: Val): string {
  switch (val.type) {
    case 'Constant':
      return `Constant(${val.value})`;
    case 'Var':
      return `Var("${val.value}")`;
    default:
      return 'Unknown';
  }
}

function formatTackyUnaryOp(op: UnaryOp): string {
  switch (op) {
    case UnaryOp.Complement:
      return 'Complement';
    case UnaryOp.Negate:
      return 'Negate';
    default:
      return 'Unknown';
  }
}

function formatTackyBinaryOp(op: BinaryOp): string {
  switch (op) {
    case BinaryOp.Add:
      return 'Add';
    case BinaryOp.Subtract:
      return 'Subtract';
    case BinaryOp.Multiply:
      return 'Multiply';
    case BinaryOp.Divide:
      return 'Divide';
    case BinaryOp.Remainder:
      return 'Remainder';
    default:
      return 'Unknown';
  }
}

// ============================================================================
// COMMAND LINE INTERFACE STRUCTURE
// ============================================================================

interface CompilerOptions {
  inputFile: string;
  outputFile: string;
  debug?: boolean;
}

function parseArgs(args: string[]): CompilerOptions | null {
  // Remove 'node' and script name from args
  const programArgs = args.slice(2);
  
  if (programArgs.length < 2 || programArgs.length > 3) {
    console.error('Usage: compiler <input_file> <output_file> [--debug]');
    return null;
  }

  const debug = programArgs.length === 3 && programArgs[2] === '--debug';

  return {
    inputFile: programArgs[0],
    outputFile: programArgs[1],
    debug
  };
}

// ============================================================================
// MAIN FUNCTION IMPLEMENTATION
// ============================================================================

function main(): void {
  const options = parseArgs(process.argv);
  
  if (!options) {
    process.exit(1);
  }

  try {
    // 1. Read input file
    const fs = require('fs');
    let sourceCode: string;
    try {
      sourceCode = fs.readFileSync(options.inputFile, 'utf8');
    } catch (error) {
      console.error(`Error reading input file '${options.inputFile}': ${(error as Error).message}`);
      process.exit(1);
    }

    // 2. Tokenize
    const tokens = lexer(sourceCode);
    const tokenStream = new TokenStream(tokens);

    // 3. Parse to AST
    const ast = parseProgram(tokenStream);
    
    // Optional: Print AST for debugging
    if (options.debug) {
      printAST(ast);
    }

    // 4. Generate tacky IR
    const tackyProgram = emitTacky(ast);
    
    // Optional: Print tacky IR for debugging
    if (options.debug) {
      printTackyIR(tackyProgram);
    }

    // 5. Generate assembly
    const assemblyCode = emitCode(tackyProgram);
    
    // 6. Generate assembly output
    const assemblyText = generateAsm(assemblyCode);
    
    // 7. Write output file
    try {
      fs.writeFileSync(options.outputFile, assemblyText);
    } catch (error) {
      console.error(`Error writing output file '${options.outputFile}': ${(error as Error).message}`);
      process.exit(1);
    }

    // Success
    if (options.debug) {
      console.log(`Successfully compiled ${options.inputFile} to ${options.outputFile}`);
    }

  } catch (error) {
    if (error instanceof LexerError || error instanceof ParseError) {
      console.error(error.message);
      process.exit(1);
    } else {
      console.error(`Compilation error: ${(error as Error).message}`);
      process.exit(1);
    }
  }
}

// ============================================================================
// EXPORTS
// ============================================================================

export {
  // Token types
  TokenType,
  Token,
  
  // AST types
  Type,
  Unop,
  Binop,
  UnopExpr,
  BinaryExpr,
  Factor,
  Expr,
  Stmt,
  Function,
  Program,
  
  // Tacky IR types
  UnaryOp,
  BinaryOp,
  Val,
  UnaryInstruction,
  BinaryInstruction,
  Instruction,
  FunctionDefinition,
  TackyProgram,
  
  // Assembly types
  Reg,
  Operand,
  UnaryOperator,
  BinaryOperator,
  Mov,
  InstructionCode,
  FunctionCode,
  Code,
  
  // Lexer
  LexerError,
  lexer,
  
  // Parser
  ParseError,
  parsePrimaryExp,
  parseExp,
  parseStatement,
  parseType,
  parseFunction,
  parseProgram,
  getTokenPrecedence,
  tokenToBinop,
  tokenToUnop,
  
  // Utility classes
  TokenStream,
  
  // CLI types
  CompilerOptions,
  parseArgs,
  
  // Tacky code generation
  generateTempVar,
  resetTempCounter,
  astUnopToTackyUnop,
  astBinopToTackyBinop,
  emitTackyExpr,
  emitTackyFactor,
  emitTackyStatement,
  emitTackyFunction,
  emitTacky,
  
  // Assembly code generation
  tackyUnopToAsmUnop,
  tackyBinopToAsmBinop,
  tackyValToOperand,
  convertTackyInstruction,
  replacePseudoRegisters,
  fixOperands,
  emitCode,
  
  // Assembly output generation
  emitOperand,
  emitInstruction,
  generateAsm,
  
  // Debug functions
  printAST,
  formatAST,
  formatFunction,
  formatType,
  formatStatement,
  formatExpr,
  formatFactor,
  formatUnop,
  formatBinop,
  printTackyIR,
  formatTackyProgram,
  formatTackyFunction,
  formatTackyInstruction,
  formatTackyVal,
  formatTackyUnaryOp,
  formatTackyBinaryOp,
  
  // Main function
  main
};

// Run main if this file is executed directly
if (require.main === module) {
  main();
}