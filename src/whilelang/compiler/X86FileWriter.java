package whilelang.compiler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jx86.lang.*;
import whilelang.ast.*;
import whilelang.util.*;

public class X86FileWriter {

	// ==========================================
	// Fields
	// ==========================================

	private final jx86.lang.Target target;
	private HashMap<String, WhileFile.MethodDecl> functions;
	private HashMap<String, WhileFile.TypeDecl> types;

	// ==========================================
	// Constructors
	// ==========================================

	public X86FileWriter(jx86.lang.Target target) {
		this.target = target;

		// Initialise register heads --- the largest register available in a
		// given family for the target platform.
		HAX = headOfFamily(Register.AX);
		HBX = headOfFamily(Register.BX);
		HCX = headOfFamily(Register.CX);
		HDX = headOfFamily(Register.DX);
		HDI = headOfFamily(Register.DI);
		HSI = headOfFamily(Register.SI);
		HBP = headOfFamily(Register.BP);
		HSP = headOfFamily(Register.SP);
		HIP = headOfFamily(Register.IP);

		// Initialise the default register pool
		REGISTER_POOL = new ArrayList<Register>();
		REGISTER_POOL.add(HAX);
		REGISTER_POOL.add(HBX);
		REGISTER_POOL.add(HCX);
		REGISTER_POOL.add(HDX);
		REGISTER_POOL.add(HDI);
		REGISTER_POOL.add(HSI);
	}

	// ==========================================
	// Public Build Method
	// ==========================================

	public X86File build(WhileFile wf) {
		X86File.Code code = new X86File.Code();
		X86File.Data data = new X86File.Data();

		this.functions = new HashMap<String, WhileFile.MethodDecl>();
		this.types = new HashMap<String, WhileFile.TypeDecl>();

		for (WhileFile.Decl declaration : wf.declarations) {
			if (declaration instanceof WhileFile.MethodDecl) {
				WhileFile.MethodDecl fd = (WhileFile.MethodDecl) declaration;
				this.functions.put(fd.name(), fd);
			} else if (declaration instanceof WhileFile.TypeDecl) {
				WhileFile.TypeDecl fd = (WhileFile.TypeDecl) declaration;
				this.types.put(fd.name(), fd);
			}
		}

		for (WhileFile.Decl d : wf.declarations) {
			if (d instanceof WhileFile.MethodDecl) {
				translate((WhileFile.MethodDecl) d, code, data);
			}
		}

		addMainLauncher(code);

		return new X86File(code, data);
	}

	// ==========================================
	// Build Helpers
	// ==========================================

	/**
	 * Translate a given function declaration into a sequence of assembly
	 * language instructions.
	 * 
	 * @param md
	 *            Method Declaration to translate.
	 * @param code
	 *            x86 code section where translation should be added.
	 * @param data
	 *            x86 data section where constants should be stored.
	 */
	public void translate(WhileFile.MethodDecl md, X86File.Code code, X86File.Data data) {
		List<Instruction> instructions = code.instructions;

		// NOTE: prefix name with "wl_" to avoid potential name clashes with
		// other symbols (e.g. as found in standard library, etc).
		instructions.add(new Instruction.Label("wl_" + md.getName()));

		// Save the old frame pointer. This is necessary because we are about to
		// overwrite the frame pointer with a new one for this method. When this
		// method returns, we need to make sure we restore the frame pointer for
		// the enclosing method.
		instructions.add(new Instruction.Reg(Instruction.RegOp.push, HBP));

		// Create new frame pointer for this function, which is stored in the
		// the HBP register. The frame pointer is a fixed reference point from
		// which we can access local variables.
		instructions.add(new Instruction.RegReg(Instruction.RegRegOp.mov, HSP, HBP));

		// Create the label for return statements. This is the point where
		// return statements will branch to, so we can avoid repeating the code
		// necessary for restoring the stack.
		String exitLabel = "label" + labelIndex++;

		// Create stack frame and ensure every variable has a known location on
		// the stack. Parameters are passed on the stack by the caller. That is,
		// in this relatively simple implementation of While, we do not attempt
		// to pass any parameters via registers (e.g. as in System V ABI).
		HashMap<String, MemoryLocation> localVariables = new HashMap<String, MemoryLocation>();
		assignCallerStackFrame(md, localVariables);
		assignCalleeStackFrame(md, localVariables);
		Context context = new Context(REGISTER_POOL, localVariables, exitLabel, code, data);

		// Create space for the stack frame, which consists of the local
		// variables. This means that new values can be safely stored in the
		// stack at the position given by HSP. That is, they won't overwrite the
		// local variables for this method.
		int widthOfLocals = determineCalleeStackFrameWidth(md);
		allocateSpaceOnStack(widthOfLocals, context);
		// translate the statements which make up the body of this method.
		translate(md.getBody(), context);

		// Add the return label. This is where any return statements in the
		// While program will jump to.
		instructions.add(new Instruction.Label(exitLabel));
		// Restore stack pointer.
		instructions.add(new Instruction.RegReg(Instruction.RegRegOp.mov, HBP, HSP));
		// Restore old frame pointer.
		instructions.add(new Instruction.Reg(Instruction.RegOp.pop, HBP));
		// Return from procedure.
		instructions.add(new Instruction.Unit(Instruction.UnitOp.ret));
	}

	/**
	 * <p>
	 * Assign every parameter and return declared in a function to an
	 * appropriate amount of space on the stack. These variables will have been
	 * preallocated by the caller and, hence, are located above the frame
	 * pointer. The amount of space required by each variable is determined by
	 * from its type.
	 * </p>
	 * 
	 * <pre>
	 *       +----------------+
	 *       |  parameter #1  |
	 *       +----------------+
	 *              ...
	 *       +----------------+
	 *       |  parameter #n  |
	 *       +----------------+
	 *       |  return value  |
	 *       +----------------+
	 *       | return address |       
	 *       +----------------+
	 *  FP-> | old frame ptr  |
	 *       +----------------+
	 * </pre>
	 * <p>
	 * Here, the parameters and return value are accessed at positive offsets
	 * from the frame pointer (whilst e.g. local variables are accessed at
	 * negative offsets).
	 * </p>
	 * 
	 * @param method
	 *            Method for which to create the stack frame
	 * @param allocation
	 *            The mapping of variable names to stack offsets. This will be
	 *            populated with those offsets for any parameters, returns or
	 *            declared local variables after this method is called.
	 * @return The number of bytes that should be allocated on the stack to
	 *         store the local variables.
	 */
	public void assignCallerStackFrame(WhileFile.MethodDecl method, Map<String, MemoryLocation> allocation) {
		// First, allocate space for parameters and return value. We need to
		// include one natural word to account for the caller return address.
		int twoWordsWidth = 2 * target.widthInBytes();
		int offset = determineCallerEnvironmentWidth(method) + twoWordsWidth;
		// Second, determine the offset for each parameter in turn
		List<WhileFile.Parameter> fun_params = method.getParameters();
		for (int i = 0; i < fun_params.size(); i++) {
			// invariant: offset >= returnAddressWidth
			WhileFile.Parameter p = fun_params.get(i);
			Type type = unwrap(p.getType());
			offset -= determineWidth(type);
			MemoryLocation loc = new MemoryLocation(type, HBP, offset);
			allocation.put(p.getName(), loc);
		}
		// Third, determine the offset for the special return value
		Type returnType = unwrap(method.getRet());
		if (!(returnType instanceof Type.Void)) {
			offset -= determineWidth(returnType);
			MemoryLocation loc = new MemoryLocation(returnType, HBP, offset);
			allocation.put("$", loc);
		}
	}

	/**
	 * <p>
	 * Assign every local variable declared in a function to an appropriate
	 * amount of space on the stack. These variables remain to be allocated by
	 * the callee and, hence, will be located below the frame pointer. The
	 * amount of space required by each variable is determined by from its type.
	 * </p>
	 * 
	 * <pre>
	 *       +----------------+
	 *       | return address |
	 *       +----------------+
	 *  FP-> | old frame ptr  |
	 *       +----------------+
	 *       |  local var #1  |
	 *       +----------------+
	 *              ...
	 *       +----------------+
	 *       |  local var #m  |
	 *       +----------------+
	 * </pre>
	 * <p>
	 * Here, the local variables are accessed at negative offsets from the frame
	 * pointer (whilst e.g. parameters are accessed at positive offsets).
	 * </p>
	 * 
	 * @param method
	 *            Method for which to create the stack frame
	 * @param allocation
	 *            The mapping of variable names to stack offsets. This will be
	 *            populated with those offsets for any parameters, returns or
	 *            declared local variables after this method is called.
	 */
	private void assignCalleeStackFrame(WhileFile.MethodDecl method, Map<String, MemoryLocation> allocation) {
		// First, we go through and determine the type of all declared
		// variables. During this process if we have two declarations for
		// variables with the same name, we retain the larger type. This
		// guarantees there is enough space for the variable in question.
		HashMap<String, Type> variables = new HashMap<String, Type>();
		extractLocalVariableTypes(method.getBody(), variables);
		// The starting offset for local variables must be below the old frame
		// pointer.
		int offset = 0;
		// Now, allocate each variable by descending from current offset.
		for (Map.Entry<String, Type> e : variables.entrySet()) {
			offset -= determineWidth(e.getValue());
			Type type = unwrap(e.getValue());
			MemoryLocation loc = new MemoryLocation(type, HBP, offset);
			allocation.put(e.getKey(), loc);
		}
	}

	// =================================================================
	// Statements
	// =================================================================

	/**
	 * Translate a list of statements into their corresponding machine code
	 * instructions. Observe that we implicitly assume all registers are
	 * available for use between statements.
	 * 
	 * @param statements
	 *            List of statements to be translated.
	 * @param localVariables
	 *            Mapping of local variable names to their byte offset from the
	 *            frame pointer.
	 * @param code
	 *            X86 code section to write machine code instructions to.
	 * @param data
	 *            Data section to store any constants required by instructions
	 *            generated for this expression (e.g. string constants)
	 */
	public void translate(List<Stmt> statements, Context context) {
		for (Stmt statement : statements) {
			translate(statement, context);
		}
	}

	/**
	 * Translate a given statement into its corresponding corresponding machine
	 * code instructions. Observe that we implicitly assume all registers are
	 * available for use between statements.
	 * 
	 * @param statement
	 *            Statement to be translated
	 */
	public void translate(Stmt statement, Context context) {
		if (statement instanceof Stmt.Assert) {
			translate((Stmt.Assert) statement, context);
		} else if (statement instanceof Stmt.Assign) {
			translate((Stmt.Assign) statement, context);
		} else if (statement instanceof Stmt.Break) {
			translate((Stmt.Break) statement, context);
		} else if (statement instanceof Stmt.Continue) {
			translate((Stmt.Continue) statement, context);
		} else if (statement instanceof Stmt.For) {
			translate((Stmt.For) statement, context);
		} else if (statement instanceof Stmt.IfElse) {
			translate((Stmt.IfElse) statement, context);
		} else if (statement instanceof Expr.Invoke) {
			// Here, we have an invocation expression being treated as a
			// statement. This means that the return value from the method is
			// being ignored. Therefore, we translate this as an expression
			// which writes the result into some register. But, we then just
			// ignore this value and it'll get overwritten later on.
			translate((Expr.Invoke) statement, null, context);
		} else if (statement instanceof Stmt.Print) {
			translate((Stmt.Print) statement, context);
		} else if (statement instanceof Stmt.Return) {
			translate((Stmt.Return) statement, context);
		} else if (statement instanceof Stmt.VariableDeclaration) {
			translate((Stmt.VariableDeclaration) statement, context);
		} else if (statement instanceof Stmt.While) {
			translate((Stmt.While) statement, context);
		} else if (statement instanceof Stmt.Switch) {
			translate((Stmt.Switch) statement, context);
		} else {
			throw new IllegalArgumentException("Unknown statement encountered: " + statement);
		}
	}

	/**
	 * Translate an assert statement in the While language into a sequence of
	 * machine instructions. This is done by calling the "assertion" method
	 * defined in the runtime.c library which, in turn, uses the standard C
	 * "assert" macro to actually do the work of checking the assertion and
	 * throwing an appropriate error if it fails.
	 * 
	 * @param statement
	 * @param localVariables
	 * @param code
	 * @param data
	 */
	public void translate(Stmt.Assert statement, Context context) {
		// We know the following must be a register location because the
		// condition returns a boolean which alwasy fits in a register.
		RegisterLocation loc = (RegisterLocation) allocateLocation(statement.getExpr(), context);
		// Translate the asserted expression and load result into target
		// register
		translate(statement.getExpr(), loc, context);
		// Now, generate a call to the assertion() function runtime.c
		makeExternalMethodCall("assertion", context, null, loc.register);
	}

	/**
	 * Translate an assignment statement in the While language into a sequence
	 * of machine instructions. The translation depends on the form of the
	 * left-hand side. That is, whether we are assigning to a variable directly,
	 * or into a compound value (e.g. a record or array).
	 * 
	 * @param statement
	 */
	public void translate(Stmt.Assign statement, Context context) {
		Expr lhs = statement.getLhs();

		// Translate assignment from HDI to left-hand side
		if (lhs instanceof Expr.Variable) {
			Expr.Variable v = (Expr.Variable) lhs;
			// Determine the offset within the stack of this local variable.
			MemoryLocation loc = context.getVariableLocation(v.getName());
			// Translate right-hand side and load result into variable location.
			translate(statement.getRhs(), loc, context);
		} else if (lhs instanceof Expr.RecordAccess) {
			// HINT: to implement this, load the address of the field being
			// assigned to into a register. Then create a memory location using
			// that register as base and translate the rhs directly into that
			// location.
			throw new IllegalArgumentException("record assignment not implemented (yet)");
		} else {
			throw new IllegalArgumentException("array assignment not implemented (yet)");
		}
	}

	public void translate(Stmt.Break statement, Context context) {
		throw new IllegalArgumentException("break statements not implemented (yet)");
	}

	public void translate(Stmt.Continue statement, Context context) {
		throw new IllegalArgumentException("continue statements not implemented (yet)");
	}

	/**
	 * Translate a for statement in the While language to a sequence of machine
	 * instructions.
	 * 
	 * @param statement
	 */
	public void translate(Stmt.For statement, Context context) {
		List<Instruction> instructions = context.instructions();
		String headLabel = freshLabel();
		String exitLabel = freshLabel();

		// Translate Variable Declaration
		translate(statement.getDeclaration(), context);

		// Start loop, and translate condition
		instructions.add(new Instruction.Label(headLabel));
		// Translate the condition expression and branch to the false label
		translateCondition(statement.getCondition(), exitLabel, context);

		// Translate Loop Body
		translate(statement.getBody(), context);

		// Translate Increment and loop around
		translate(statement.getIncrement(), context);
		instructions.add(new Instruction.Addr(Instruction.AddrOp.jmp, headLabel));

		// Exit ...
		instructions.add(new Instruction.Label(exitLabel));
	}

	/**
	 * Translate an if statement in the While language to a sequence of machine
	 * instructions.
	 * 
	 * @param statement
	 * @param localVariables
	 * @param code
	 * @param data
	 */
	public void translate(Stmt.IfElse statement, Context context) {
		List<Instruction> instructions = context.instructions();
		boolean hasFalseBranch = statement.getFalseBranch().size() > 0;
		String exitLabel = freshLabel();
		String falseLabel = hasFalseBranch ? freshLabel() : exitLabel;

		// Translate the condition expression and branch to the false label
		translateCondition(statement.getCondition(), falseLabel, context);

		// Translate true branch
		translate(statement.getTrueBranch(), context);
		instructions.add(new Instruction.Addr(Instruction.AddrOp.jmp, exitLabel));
		// Translate false branch (if applicable)
		if (hasFalseBranch) {
			instructions.add(new Instruction.Label(falseLabel));
			translate(statement.getFalseBranch(), context);
		}
		// done
		instructions.add(new Instruction.Label(exitLabel));
	}

	/**
	 * Translate a print statement in the While language to a sequence of
	 * machine instructions.
	 * 
	 * @param statement
	 * @param localVariables
	 * @param code
	 * @param data
	 */
	public void translate(Stmt.Print statement, Context context) {
		// Determine type of expression so as to determine appropriate print
		// call.
		Type type = unwrap(statement.getExpr().attribute(Attribute.Type.class).type);
		String name;
		if (type instanceof Type.Bool) {
			name = "prnbool";
		} else if (type instanceof Type.Int) {
			name = "prnint";
		} else if (type instanceof Type.Strung) {
			name = "prnstr";
		} else if (isIntArray(type)) {
			name = "prnintn";
		} else {
			// We basically just ignore all other kinds at this point
			return;
		}
		// Allocate space for the result (which we now know will fit into a
		// single register).
		RegisterLocation loc = (RegisterLocation) allocateLocation(statement.getExpr(), context);
		// Translate the target expression and load result into target register
		translate(statement.getExpr(), loc, context);
		// Call the appropriate method form
		makeExternalMethodCall(name, context, null, loc.register);
	}

	/**
	 * <p>
	 * Translate a return statement in the While language to a sequence of
	 * machine instructions. Although there is a machine instruction for
	 * returning from a method, we don't use that here. The reason is that,
	 * before returning from the method, we must restore the stack frame as it
	 * was when the method was called. The code for doing this is generated at
	 * the end of the method itself and, therefore, we simply branch to that
	 * point rather than repeating it here.
	 * </p>
	 * <p>
	 * In the case of a return value being supplied, we must write that into the
	 * appropriate place in the stack frame. This has been given a pretend
	 * variable name, "$", which we can simply read out from the local variables
	 * map.
	 * </p>
	 * 
	 * @param statement
	 * @param localVariables
	 * @param code
	 * @param data
	 */
	public void translate(Stmt.Return statement, Context context) {
		List<Instruction> instructions = context.instructions();
		Expr rv = statement.getExpr();

		if (rv != null) {
			// Determine the offset within the stack of this local variable.
			MemoryLocation loc = context.getVariableLocation("$");
			// Translate right-hand side and load into variable location
			translate(rv, loc, context);
		}

		// Finally, we branch to the end of the function where the code
		// necessary for restoring the stack is located.
		instructions.add(new Instruction.Addr(Instruction.AddrOp.jmp, context.exitLabel()));
	}

	/**
	 * Translate a variable declaration in the While language to a sequence of
	 * machine instructions. This will only actually correspond to any
	 * instructions if the declaration includes an initialiser. In such case,
	 * the generated code is the same as for a simple assignment statement.
	 * 
	 * @param statement
	 * @param localVariables
	 * @param code
	 * @param data
	 */
	public void translate(Stmt.VariableDeclaration statement, Context context) {
		Expr initialiser = statement.getExpr();

		if (initialiser != null) {
			// Determine the offset within the stack of this local variable.
			MemoryLocation loc = context.getVariableLocation(statement.getName());
			// Translate the right-hand side and load result into target
			// register
			translate(initialiser, loc, context);
		}
	}

	/**
	 * Translate a while statement in the While language to a sequence of
	 * machine instructions.
	 * 
	 * @param statement
	 * @param localVariables
	 * @param code
	 * @param data
	 */
	public void translate(Stmt.While statement, Context context) {
		throw new IllegalArgumentException("while loops not implemented (yet)");
	}

	/**
	 * Translate a switch statement in the While language to a sequence of
	 * machine instructions.
	 * 
	 * @param statement
	 * @param localVariables
	 * @param code
	 * @param data
	 */
	public void translate(Stmt.Switch statement, Context context) {
		List<Instruction> instructions = context.instructions();
		//
		Location[] tmps = allocateLocations(context, statement.getExpr(), statement.getExpr());
		// The exit label will represent the exit point from the switch
		// statement. Any cases which end in a break will branch to it.
		String exitLabel = freshLabel();
		// Translate the expression we are switching on, and place result
		// into the target register.
		translate(statement.getExpr(), tmps[0], context);
		// Lock the target register here. This is necessary because we will only
		// evaluate this once, and we want to retain its value across each of
		// the comparisons needed for the cases.
		context = context.lockLocation(tmps[0]);
		// Translate each of the case blocks. For simplicity, we're just
		// going to use a chain of conditional branches. This is not optimal,
		// and it would be better to use a jump table in situations where we can
		// (e.g. for integer values). However, in the general case (e.g. when
		// switching on records), we cannot use a jump table anyway.
		for (Stmt.Case c : statement.getCases()) {
			String nextLabel = freshLabel();
			Expr constant = c.getValue();
			if (constant != null) {
				// Not a default block
				translate(c.getValue(), tmps[1], context);
				bitwiseEquality(tmps[0], tmps[1], nextLabel, context);
			}
			// FIXME: need to handle break and continue statements!
			translate(c.getBody(), context);
			instructions.add(new Instruction.Label(nextLabel));
		}
		// Finally, add the exit label
		instructions.add(new Instruction.Label(exitLabel));
		// Free up space used for value being switch upon
		freeLocations(context, tmps);
	}

	// =================================================================
	// Conditions
	// =================================================================

	/**
	 * Translate a condition expression and, if it is false, branch to a given
	 * false destination. Otherwise, execution continues to the following
	 * instruction.
	 * 
	 * @param e
	 *            A binary relational expression
	 * @param falseLabel
	 *            The branch destination for the case the equality does not
	 *            hold.
	 */
	public void translateCondition(Expr e, String falseLabel, Context context) {
		//
		if (e instanceof Expr.Unary) {
			translateLogicalNotCondition((Expr.Unary) e, falseLabel, context);
		} else if (e instanceof Expr.Binary) {
			Expr.Binary b = (Expr.Binary) e;
			switch (b.getOp()) {
			case AND:
				translateShortCircuitConjunctCondition((Expr.Binary) e, falseLabel, context);
				break;
			case OR:
				translateShortCircuitDisjunctCondition((Expr.Binary) e, falseLabel, context);
				break;
			case EQ:
			case NEQ:
				translateEqualityCondition((Expr.Binary) e, falseLabel, context);
				break;
			case LT:
			case LTEQ:
			case GTEQ:
			case GT:
				translateRelationalCondition((Expr.Binary) e, falseLabel, context);
				break;
			default:
				throw new IllegalArgumentException("invalid binary condition");
			}
		} else {
			List<Instruction> instructions = context.instructions();
			// Need to deal with the general case here. For example, the
			// expression could be a variable or field load or the result of a
			// method invocation. Eitherway, we do know that the result will fit
			// into a register.
			RegisterLocation loc = (RegisterLocation) allocateLocation(e, context);
			//
			translate(e, loc, context);
			//
			instructions.add(new Instruction.ImmReg(Instruction.ImmRegOp.cmp, 0, loc.register));
			instructions.add(new Instruction.Addr(Instruction.AddrOp.jz, falseLabel));
		}
	}

	/**
	 * Translate a logical NOT expression (i.e. '!').
	 * 
	 * @param e
	 *            A binary logical expression
	 * @param falseLabel
	 *            The branch destination for the case the equality does not
	 *            hold.
	 */
	public void translateLogicalNotCondition(Expr.Unary e, String falseLabel, Context context) {
		List<Instruction> instructions = context.instructions();
		if (e.getOp() != Expr.UOp.NOT) {
			throw new IllegalArgumentException("invalid unary condition");
		}
		// What we do here, is redirect the branching. So, the subcondition ends
		// up branching to the falseLabel if it evaluates to true, and
		// vice-versa.
		String trueLabel = freshLabel();
		translateCondition(e.getExpr(), trueLabel, context);
		instructions.add(new Instruction.Addr(Instruction.AddrOp.jmp, falseLabel));
		instructions.add(new Instruction.Label(trueLabel));
	}

	/**
	 * Translate a logical OR expression (i.e. '||'). This supports
	 * short-circuiting evaluation.
	 * 
	 * @param e
	 *            A binary logical expression
	 * @param falseLabel
	 *            The branch destination for the case the equality does not
	 *            hold.
	 */
	public void translateShortCircuitDisjunctCondition(Expr.Binary e, String falseLabel, Context context) {
		List<Instruction> instructions = context.instructions();
		String nextLabel = freshLabel();
		String exitLabel = freshLabel();
		translateCondition(e.getLhs(), nextLabel, context);
		instructions.add(new Instruction.Addr(Instruction.AddrOp.jmp, exitLabel));
		instructions.add(new Instruction.Label(nextLabel));
		translateCondition(e.getRhs(), falseLabel, context);
		instructions.add(new Instruction.Label(exitLabel));
	}

	/**
	 * Translate a logical AND expression (i.e. '||'). This supports
	 * short-circuiting evaluation.
	 * 
	 * @param e
	 *            A binary logical expression
	 * @param falseLabel
	 *            The branch destination for the case the equality does not
	 *            hold.
	 */
	public void translateShortCircuitConjunctCondition(Expr.Binary e, String falseLabel, Context context) {
		translateCondition(e.getLhs(), falseLabel, context);
		translateCondition(e.getRhs(), falseLabel, context);
	}

	/**
	 * Translate one of the four relational comparators (i.e. <,<=,>= and >).
	 * These comparators are relatively straightforward because they can only be
	 * applied to integer (i.e. primitive) values.
	 * 
	 * @param e
	 *            A binary relational expression
	 * @param falseLabel
	 *            The branch destination for the case the equality does not
	 *            hold.
	 */
	public void translateRelationalCondition(Expr.Binary e, String falseLabel, Context context) {
		List<Instruction> instructions = context.instructions();
		// Translate left-hand side (which must be a register location)
		Location[] tmps = (Location[]) allocateLocations(context, e.getLhs(), e.getRhs());
		translate(e.getLhs(), tmps[0], context);
		context = context.lockLocation(tmps[0]);
		// Translate right-hand side (which must be a register location)
		translate(e.getRhs(), tmps[1], context);
		// NOTE: it is not a mistake that rhs is in the left operand
		// position. This is because we're in AT&T syntax!
		Register lhsReg = ((RegisterLocation) tmps[0]).register;
		Register rhsReg = ((RegisterLocation) tmps[1]).register;
		instructions.add(new Instruction.RegReg(Instruction.RegRegOp.cmp, rhsReg, lhsReg));
		//
		switch (e.getOp()) {
		case LT:
			instructions.add(new Instruction.Addr(Instruction.AddrOp.jge, falseLabel));
			break;
		case LTEQ:
			instructions.add(new Instruction.Addr(Instruction.AddrOp.jg, falseLabel));
			break;
		case GT:
			instructions.add(new Instruction.Addr(Instruction.AddrOp.jle, falseLabel));
			break;
		case GTEQ:
			instructions.add(new Instruction.Addr(Instruction.AddrOp.jl, falseLabel));
			break;
		default:
			throw new IllegalArgumentException("Unknown binary operator: " + e);
		}
	}

	/**
	 * Translate the equality comparator for values of a given type. In the case
	 * the equality holds, control will continue to the next instruction in
	 * sequence. Otherwise, it will branch to a given false destination.
	 * 
	 * @param type
	 *            The type of values being compared.
	 * @param falseLabel
	 *            The branch destination for the case the equality does not
	 *            hold.
	 */
	public void translateEqualityCondition(Expr.Binary e, String falseLabel, Context context) {
		List<Instruction> instructions = context.instructions();
		// TODO: we're currently making an assumption here that the lhs and rhs
		// types align. If they don't the behaviour is undefined.
		//
		// Translate left-hand side
		Location[] tmps = allocateLocations(context, e.getLhs(), e.getRhs());
		translate(e.getLhs(), tmps[0], context);
		context = context.lockLocation(tmps[0]);
		// Translate right-hand side
		translate(e.getRhs(), tmps[1], context);
		//
		// Perform a bitwise comparison of the two data chunks
		if (e.getOp() == Expr.BOp.EQ) {
			bitwiseEquality(tmps[0], tmps[1], falseLabel, context);
		} else {
			String trueLabel = freshLabel();
			bitwiseEquality(tmps[0], tmps[1], trueLabel, context);
			instructions.add(new Instruction.Addr(Instruction.AddrOp.jmp, falseLabel));
			instructions.add(new Instruction.Label(trueLabel));
		}
		freeLocations(context, tmps);
	}

	// =================================================================
	// Expressions
	// =================================================================

	/**
	 * Translate a given expression into the corresponding machine code
	 * instructions. The expression is expected to return its result in the
	 * target register or, if that is null, on the stack. The set of free
	 * registers is provided to identify the pool from which target registers
	 * can be taken.
	 * 
	 * @param expression
	 *            Expression to be translated.
	 * @param target
	 *            Location to store result in.
	 */
	public void translate(Expr expression, Location target, Context context) {
		if (expression instanceof Expr.Binary) {
			if (target instanceof RegisterLocation) {
				translate((Expr.Binary) expression, (RegisterLocation) target, context);
			} else {
				translateViaRegister(expression, (MemoryLocation) target, context);
			}
		} else if (expression instanceof Expr.Constant) {
			translate((Expr.Constant) expression, target, context);
		} else if (expression instanceof Expr.Invoke) {
			translate((Expr.Invoke) expression, target, context);
		} else if (expression instanceof Expr.RecordAccess) {
			translate((Expr.RecordAccess) expression, target, context);
		} else if (expression instanceof Expr.RecordConstructor) {
			translate((Expr.RecordConstructor) expression, (MemoryLocation) target, context);
		} else if (expression instanceof Expr.Unary) {
			if (target instanceof RegisterLocation) {
				translate((Expr.Unary) expression, (RegisterLocation) target, context);
			} else {
				translateViaRegister(expression, (MemoryLocation) target, context);
			}
		} else if (expression instanceof Expr.Variable) {
			translate((Expr.Variable) expression, target, context);
		} else {
			throw new IllegalArgumentException("Unknown expression encountered: " + expression);
		}
	}

	/**
	 * Translate a given expression via a register target. Thus, the expression
	 * is first loaded into a temporary register and then bit-blasted into the
	 * target location.
	 * 
	 * @param e
	 *            Expression to be translate
	 * @param target
	 *            target location in memory
	 * @param context
	 */
	public void translateViaRegister(Expr e, MemoryLocation target, Context context) {
		MemoryLocation tloc = (MemoryLocation) target;
		RegisterLocation rloc = context.selectFreeRegister(target.type());
		translate(e, rloc, context);
		bitwiseCopy(rloc, tloc, context);
	}

	/**
	 * Translate a binary expression into the corresponding machine code
	 * instructions.
	 * 
	 * @param expression
	 *            Expression to be translated.
	 * @param target
	 *            Location to store result in (either register or stack
	 *            location)
	 */
	public void translate(Expr.Binary e, RegisterLocation target, Context context) {
		switch (e.getOp()) {
		case ADD:
		case SUB:
		case MUL:
		case DIV:
		case REM:
			translateArithmeticOperator(e, target, context);
			break;
		case AND:
		case OR:
		case EQ:
		case NEQ:
		case LT:
		case LTEQ:
		case GT:
		case GTEQ:
			translateBinaryCondition(e, target, context);
			break;
		default:
			throw new IllegalArgumentException("Unknown binary operator: " + e);
		}
	}

	/**
	 * Translate a binary condition when used in the context of a general
	 * expression. This means that the condition must load either a zero or one
	 * into the target register.
	 * 
	 * @param e
	 *            Expression to be translated.
	 * @param target
	 *            Location to store result in (either register or stack
	 *            location)
	 */
	public void translateBinaryCondition(Expr.Binary e, RegisterLocation target, Context context) {
		List<Instruction> instructions = context.instructions();
		//
		String falseLabel = freshLabel();
		String exitLabel = freshLabel();
		translateCondition(e, falseLabel, context);
		instructions.add(new Instruction.ImmReg(Instruction.ImmRegOp.mov, 1, target.register));
		instructions.add(new Instruction.Addr(Instruction.AddrOp.jmp, exitLabel));
		instructions.add(new Instruction.Label(falseLabel));
		instructions.add(new Instruction.ImmReg(Instruction.ImmRegOp.mov, 0, target.register));
		instructions.add(new Instruction.Label(exitLabel));
	}

	/**
	 * Translate one of the arithmetic operators (i.e. +,-,*, etc). These are
	 * relatively straightforward because they can only be applied to integer
	 * (i.e. primitive) data.
	 * 
	 * @param e
	 *            Expression to be translated.
	 * @param target
	 *            Location to store result in (either register or stack
	 *            location)
	 */
	public void translateArithmeticOperator(Expr.Binary e, RegisterLocation target, Context context) {
		List<Instruction> instructions = context.instructions();
		// Translate lhs and store result in the target register.
		translate(e.getLhs(), target, context);
		// We need to lock the target register here because it's value cannot be
		// overwritten in subsequent operations.
		context = context.lockLocation(target);
		// Determine register into which to store rhs. Note that we don't need
		// to lock this register because it can still be used as a temporary
		// when translating the right-hand side.
		RegisterLocation rhs = (RegisterLocation) allocateLocation(e.getRhs(), context);
		// Translate rhs and store result in temporary register
		translate(e.getRhs(), rhs, context);
		// UNlock the target location. This is necessary for division to ensure
		// that the target is not saved to the stack
		context = context.unlockLocation(target);
		// Finally, perform the binary operation.
		// Translate one of the arithmetic operators
		switch (e.getOp()) {
		case ADD:
			instructions.add(new Instruction.RegReg(Instruction.RegRegOp.add, rhs.register, target.register));
			break;
		case SUB:
			instructions.add(new Instruction.RegReg(Instruction.RegRegOp.sub, rhs.register, target.register));
			break;
		case MUL:
			instructions.add(new Instruction.RegReg(Instruction.RegRegOp.imul, rhs.register, target.register));
			break;
		case DIV:
			// The idiv instruction is curious because you cannot control where
			// the result is stored. That is, the result is always stored into
			// the hdx:hax register pairing (where hdx = remainder, hax =
			// quotient).
			saveRegistersIfUsed(Arrays.asList(HAX, HDX), context);
			if (rhs.register == HAX || rhs.register == HDX) {
				// For HAX, can resolve this using an xchg; for HDX, unsure.
				throw new RuntimeException("*** REGISTER CONFLICT ON DIVISION");
			}
			instructions.add(new Instruction.RegReg(Instruction.RegRegOp.mov, target.register, HAX));
			// The following is needed for signed extension. This is broken on
			// 32bit architectures :(
			instructions.add(new Instruction.Unit(Instruction.UnitOp.cqto));
			instructions.add(new Instruction.Reg(Instruction.RegOp.idiv, rhs.register));
			instructions.add(new Instruction.RegReg(Instruction.RegRegOp.mov, HAX, target.register));
			restoreRegistersIfUsed(Arrays.asList(HAX, HDX), context);
			break;
		case REM:
			// The idiv instruction is curious because you cannot control where
			// the result is stored. That is, the result is always stored into
			// the hdx:has register pairing (where hdx = remainder, hax =
			// quotient).
			saveRegistersIfUsed(Arrays.asList(HAX, HDX), context);
			if (rhs.register == HAX || rhs.register == HDX) {
				// Can resolve this using an xchg; for HDX, unsure.
				throw new RuntimeException("*** REGISTER CONFLICT ON REMAINDER");
			}
			instructions.add(new Instruction.RegReg(Instruction.RegRegOp.mov, target.register, HAX));
			// The following is needed for signed extension. This is broken on
			// 32bit artchitectures :(
			instructions.add(new Instruction.Unit(Instruction.UnitOp.cqto));
			instructions.add(new Instruction.Reg(Instruction.RegOp.idiv, rhs.register));
			instructions.add(new Instruction.RegReg(Instruction.RegRegOp.mov, HDX, target.register));
			restoreRegistersIfUsed(Arrays.asList(HAX, HDX), context);
			break;
		default:
			throw new IllegalArgumentException("Unknown binary operator: " + e);
		}
	}

	public void translate(Expr.Constant e, Location target, Context context) {
		translateConstant(e.getValue(), target, context);
	}

	public void translateConstant(Object value, Location target, Context context) {
		List<Instruction> instructions = context.instructions();
		if (value instanceof HashMap) {
			HashMap<String, Object> record = (HashMap<String, Object>) value;
			MemoryLocation recordLoc = (MemoryLocation) target;
			Type.Record recordType = (Type.Record) target.type();
			int offset = 0;
			for (Pair<Type, String> p : recordType.getFields()) {
				Type fieldType = p.first();
				MemoryLocation fieldLoc = new MemoryLocation(fieldType, recordLoc.base, recordLoc.offset + offset);
				translateConstant(record.get(p.second()), fieldLoc, context);
				offset = offset + determineWidth(fieldType);
			}
		} else {
			RegisterLocation tmp;
			// We need to identify a register location into which to load this
			// constant value. If the current target is a register location,
			// then fine. Otherwise, we need to find a free register.
			if (target instanceof RegisterLocation) {
				tmp = (RegisterLocation) target;
			} else {
				tmp = context.selectFreeRegister(target.type);
			}
			if (value instanceof Boolean) {
				Boolean b = (Boolean) value;
				instructions.add(new Instruction.ImmReg(Instruction.ImmRegOp.mov, b ? 1 : 0, tmp.register));
			} else if (value instanceof Character) {
				Character c = (Character) value;
				instructions.add(new Instruction.ImmReg(Instruction.ImmRegOp.mov, c, tmp.register));
			} else if (value instanceof Integer) {
				Integer i = (Integer) value;
				instructions.add(new Instruction.ImmReg(Instruction.ImmRegOp.mov, i, tmp.register));
			} else if (value instanceof String) {
				String i = (String) value;
				List<Constant> constants = context.constants();
				// First, check to see whether string already allocated as a
				// constant. This is really important, since we assume that
				// identical string values have the same address in memory.
				String label = null;
				for (Constant c : constants) {
					if (c instanceof Constant.String) {
						Constant.String str = (Constant.String) c;
						if (str.value.equals(i)) {
							// match
							label = str.label;
							break;
						}
					}
				}
				if (label == null) {
					// We didn't find a match, so allocate a new string constant
					label = "str" + constants.size();
					constants.add(new Constant.String(label, i));
				}
				instructions.add(new Instruction.AddrRegReg(Instruction.AddrRegRegOp.lea, label, HIP, tmp.register));
			} else {
				throw new IllegalArgumentException("Unknown constant encountered: " + value);
			}
			// Copy from tmp to target. If they are the same, this will be a
			// no-operation.
			bitwiseCopy(tmp, target, context);
		}
	}

	/**
	 * /
	 * <p>
	 * Translate an invocation expression. This requires setting up the caller
	 * environment appropriately so that parameters can be passed into the
	 * called method, and returns can be passed back. The amount of space
	 * required by each variable is determined by from its type.
	 * </p>
	 *
	 * <pre>
	 *       +----------------+
	 *       |  parameter #1  |
	 *       +----------------+
	 *              ...
	 *       +----------------+
	 *       |  parameter #n  |
	 *       +----------------+
	 *  SP-> |  return value  |
	 *       +----------------+
	 * </pre>
	 * <p>
	 * Here, the parameters and return value are accessed at positive offsets
	 * from the stack pointer (whilst e.g. local variables are accessed at
	 * negative offsets).
	 * </p>
	 *
	 * @param e
	 *            Expression to be translated.
	 * @param target
	 *            Location to store result in (either register or stack
	 *            location)
	 */
	public void translate(Expr.Invoke e, Location target, Context context) {
		List<Instruction> instructions = context.instructions();

		// Save all used registers to the stack as part of the caller-save
		// calling convention.
		int savedRegistersWidth = saveRegistersIfUsed(REGISTER_POOL, context);

		// First, determine the amount of space to reserve on the stack for
		// parameters and the return value (if applicable).
		WhileFile.MethodDecl md = functions.get(e.getName());
		int callerEnvWidth = determineCallerEnvironmentWidth(md);

		// Second, create space on the stack for parameters and return value
		callerEnvWidth = allocateSpaceOnStack(callerEnvWidth, context);

		// Third, translate invocation arguments and load them onto the stack.
		// We have to go "backwards" because we are moving up the stack
		// allocated space. Furthermore, we must start above the space allocated
		// for the return value.
		int offset = determineWidth(md.getRet());
		List<Expr> arguments = e.getArguments();
		List<WhileFile.Parameter> parameters = md.getParameters();
		for (int i = arguments.size() - 1; i >= 0; --i) {
			WhileFile.Parameter parameter = parameters.get(i);
			Type type = unwrap(parameter.getType());
			Expr argument = arguments.get(i);
			Location tmp = new MemoryLocation(type, HSP, offset);
			translate(argument, tmp, context);
			offset += determineWidth(parameter.getType());
		}

		// Fourth, actually invoke the function
		String fn_name = "wl_" + md.getName();
		instructions.add(new Instruction.Addr(Instruction.AddrOp.call, fn_name));

		// Free space previously allocated on the stack
		freeSpaceOnStack(callerEnvWidth, context);

		// Restore all used registers
		restoreRegistersIfUsed(REGISTER_POOL, context);

		Type returnType = unwrap(md.getRet());
		if (target != null && !(returnType instanceof Type.Void)) {
			// Finally, copy return value into its destination. It may seem odd
			// that we do this here, *after* we've freed space on the stack,
			// especially as we must then account for this discrepancy. However,
			// we must do it here, as the target location may have been relative
			// to the stack pointer on entry to this method and, if so, would be
			// pointing to the wrong place before we adjusted the stack.
			MemoryLocation tmp = new MemoryLocation(returnType, HSP, -(callerEnvWidth + savedRegistersWidth));
			bitwiseCopy(tmp, target, context);
		}
	}

	public void translate(Expr.RecordAccess e, Location target, Context context) {
		// Determine the field offset
		Type.Record type = (Type.Record) unwrap(e.getSource().attribute(Attribute.Type.class).type);
		int offset = getFieldOffset(type, e.getName());
		// Translate source expression into a temporary stack location. This is
		// unfortunately a little inefficient in some cases, as we could
		// potentially avoid all memory usage. But, it will do for now!
		MemoryLocation recordLocation = (MemoryLocation) allocateLocation(e.getSource(), context);
		translate(e.getSource(), recordLocation, context);
		// Finally, copy bits into target location
		MemoryLocation fieldLocation = new MemoryLocation(target.type(), recordLocation.base, offset);
		bitwiseCopy(fieldLocation, target, context);
		//
		freeLocations(context, recordLocation);
	}

	/**
	 * <p>
	 * Translate a record constructor. This is challenging because we have only
	 * a single target register in which to place an entire record. Obviously,
	 * this is not possible. Instead, we place a pointer to the record into the
	 * target register.
	 * </p>
	 * <p>
	 * We choose to avoid allocating the record on the heap here and, instead,
	 * allocate it in some temporary storage on the stack. Roughly, the layout
	 * looks like this after this is completed:
	 * </p>
	 * 
	 * <pre>
	 *      +----------------+
	 * FP-> | old frame ptr  |
	 *      +----------------+
	 *      |  local var #1  |
	 *      +----------------+
	 *             ...
	 *      +----------------+
	 *      |  local var #m  |
	 *      +----------------+
	 *             ...
	 *      +----------------+
	 *      |    padding     |
	 *      +----------------+ 
	 *      |    field #n    |
	 *      +----------------+
	 *             ...
	 *      +----------------+
	 * SP-> |    field 1     |
	 * 	    +----------------+
	 * </pre>
	 * 
	 * As when allocating space on the stack for the caller environment, we must
	 * ensure the stack remains 16byte aligned. This means there may be one or
	 * more words of padding inserted. For convenience, this is done at the
	 * highest point in the stack. At the end, the target register will match
	 * the Stack Pointer (i.e. at the base address of the record).
	 * 
	 * @param e
	 *            Expression to be translated.
	 * @param target
	 *            Location to store result in (either register or stack
	 *            location)
	 */
	public void translate(Expr.RecordConstructor e, MemoryLocation target, Context context) {
		// Create space on the stack for the resulting record
		Type.Record type = (Type.Record) unwrap(e.attribute(Attribute.Type.class).type);
		// Translate fields in the order and write into the preallocated stack
		// space.
		int offset = 0;
		List<Pair<String, Expr>> fields = e.getFields();
		for (int i = 0; i != fields.size(); ++i) {
			Pair<String, Expr> p = fields.get(i);
			Pair<Type, String> f = type.getFields().get(i);
			Type fieldType = unwrap(f.first());
			MemoryLocation fieldLoc = new MemoryLocation(fieldType, target.base, target.offset + offset);
			translate(p.second(), fieldLoc, context);
			offset += determineWidth(fieldType);
		}
	}

	public void translate(Expr.Unary e, RegisterLocation target, Context context) {
		List<Instruction> instructions = context.instructions();

		// First, translate lhs and store result in the target register.
		translate(e.getExpr(), target, context);

		// Finally, perform the binary operation.
		switch (e.getOp()) {
		case NOT:
			// First, perform logical not of all bites
			instructions.add(new Instruction.Reg(Instruction.RegOp.not, target.register));
			// Second, ensure only bit 1 may be set
			instructions.add(new Instruction.ImmReg(Instruction.ImmRegOp.and, 1, target.register));
			break;
		case NEG:
			instructions.add(new Instruction.Reg(Instruction.RegOp.neg, target.register));
			break;
		default:
			throw new IllegalArgumentException("Unknown unary operator: " + e);
		}
	}

	public void translate(Expr.Variable e, Location target, Context context) {
		// There are two cases we need to consider here: primitive values; and,
		// compound values. For the former, we load their value directly into
		// the target register. For the latter, we load a pointer to their value
		// directly into the target register.

		// Determine the offset within the stack of this local variable.
		MemoryLocation loc = context.getVariableLocation(e.getName());

		// Copy data from variable location into target location.
		bitwiseCopy(loc, target, context);
	}

	// ==========================================
	// Other Helpers
	// ==========================================

	/**
	 * Make an external method call, passing in zero or more arguments and
	 * potentially returning a value. Note that the return register should be a
	 * free register, whilst the argument registers may or may not be (and if
	 * not they will not be preserved).
	 * 
	 * @param name
	 *            The external method to call
	 * @param code
	 *            Code section to add on those instructions corresponding to
	 *            this expression
	 * @param returnTarget
	 *            The register into which the result should be placed, or null
	 *            if no result.
	 * @param arguments
	 *            Zero or more arguments.
	 */
	public void makeExternalMethodCall(String name, Context context, Register returnTarget, Register... arguments) {
		// The argument registers lists the order in which arguments are passed
		// to external registers on the call stack.
		final Register[] parameterTargets = { HDI, HSI, HDX, HCX };
		List<Instruction> instructions = context.instructions();

		// Save all used registers onto the stack. A used register is one which
		// is not in the list of freeRegisters and, hence, whose value must be
		// preserved across the method call. In principle, we could optimise
		// this further as some registers are known to be callee-saved.
		saveRegistersIfUsed(REGISTER_POOL, context);

		// At this point, we now configure the register parameters to pass into
		// the external method. We have to be careful if the parameter target
		// at a given pointer overlaps with one (or more) of remaining argument
		// registers. When this happens, we swap the contents of the register
		// argument with that being overlapped.
		for (int i = 0; i != arguments.length; ++i) {
			Register argument = arguments[i];
			Register target = parameterTargets[i];
			if (argument == target) {
				// In the case that the argument register matches the target
				// register, then we don't need to do anything at all!
			} else {
				// Now, we need to look for a potential conflict between the
				// current target and one (or more) of the remaining arguments;
				Register conflict = null;
				for (int j = i + 1; j < arguments.length; ++j) {
					Register other = arguments[j];
					if (other == target) {
						// Yes, we have a conflict. We need to substitute
						// through with the current argument register to ensure
						// the swap operation is safe.
						conflict = other;
						arguments[j] = argument;
					}
				}
				if (conflict != null) {
					// In this case, a conflict was detected. Therefore, we
					// simply swap the contents of the current argument with
					// that of the conflicting register. This is guaranteed to
					// be safe since we know argument != target.
					instructions.add(new Instruction.RegReg(Instruction.RegRegOp.xchg, argument, conflict));
				} else {
					// In this case, no conflict occurred. Therefore, we simply
					// move the operand register into the desired target
					// register for the given calling convention.
					instructions.add(new Instruction.RegReg(Instruction.RegRegOp.mov, argument, target));
				}
			}
		}
		// Ok, we're all done.
		instructions.add(new Instruction.Addr(Instruction.AddrOp.call, externalSymbolName(name)));
		if (returnTarget != null) {
			instructions.add(new Instruction.RegReg(Instruction.RegRegOp.mov, HAX, returnTarget));
		}
		restoreRegistersIfUsed(REGISTER_POOL, context);
	}

	/**
	 * Save all registers in a given pool which are in use on the stack. This is
	 * done (for example) as part of the caller-save protocol. The set of used
	 * registers is determined as the set of registers which are not "Free".
	 * 
	 * @param pool
	 *            The pool of registers which we want to save (if necessary)
	 */
	public int saveRegistersIfUsed(List<Register> pool, Context context) {
		final int oneWordWidth = target.widthInBytes();
		List<Instruction> instructions = context.instructions();
		List<Register> usedRegisters = context.getUsedRegisters(pool);
		int width = usedRegisters.size() * oneWordWidth;
		width = allocateSpaceOnStack(width, context);
		for (int i = 0; i != usedRegisters.size(); ++i) {
			int offset = i * oneWordWidth;
			Register r = usedRegisters.get(i);
			instructions.add(new Instruction.RegImmInd(Instruction.RegImmIndOp.mov, r, offset, HSP));
		}
		return width;
	}

	/**
	 * Restore all registers in a given pool which were in use on the stack.
	 * This is done (for example) as part of the caller-save protocol. The set
	 * of used registers is determined as the set of registers which are not
	 * "Free".
	 * 
	 * @param pool
	 *            The array of registers which we want to restore (if necessary)
	 */
	public void restoreRegistersIfUsed(List<Register> pool, Context context) {
		List<Instruction> instructions = context.instructions();
		final int oneWordWidth = target.widthInBytes();
		List<Register> usedRegisters = context.getUsedRegisters(pool);
		for (int i = 0; i != usedRegisters.size(); ++i) {
			int offset = i * oneWordWidth;
			Register r = usedRegisters.get(i);
			instructions.add(new Instruction.ImmIndReg(Instruction.ImmIndRegOp.mov, offset, HSP, r));
		}
		freeSpaceOnStack(usedRegisters.size() * oneWordWidth, context);
	}

	/**
	 * <p>
	 * Allocate a location for storing the result of an expression. For results
	 * which fit in a single register, the allocated location will be one of the
	 * available free registers. For results which don't fit in a single
	 * register (e.g. record types), the location will be allocated on the
	 * stack.
	 * </p>
	 * 
	 * <p>
	 * <b>NOTE:</b>In the event that a free register is allocated, this register
	 * will not be locked. The reason for this is that the register could still
	 * be useful as an intermediate when translating the given expression.
	 * </p>
	 * <p>
	 * <b>NOTE:</b>Locations allocated using this method should be explicitly
	 * freed once they are no longer needed. This is to ensure that any stack
	 * space allocated is eventually released.
	 * </p>
	 * 
	 * @param e
	 *            Expression whose result we are allocating a location for.
	 * @param context
	 */
	public Location allocateLocation(Expr e, Context context) {
		return allocateLocations(context, e)[0];
	}

	/**
	 * <p>
	 * Allocate a location for storing the results of one or more expressions.
	 * For results which fit in a single register, the allocated location will
	 * be one of the available free registers. For results which don't fit in a
	 * single register (e.g. record types), the location will be allocated on
	 * the stack.
	 * </p>
	 * 
	 * <p>
	 * <b>NOTE:</b>In the event that a free register is allocated, this register
	 * will not be locked. The reason for this is that the register could still
	 * be useful as an intermediate when translating the given expression.
	 * </p>
	 * <p>
	 * <b>NOTE:</b>Locations allocated using this method should be explicitly
	 * freed once they are no longer needed. This is to ensure that any stack
	 * space allocated is eventually released.
	 * </p>
	 * 
	 * @param e
	 *            Expression whose result we are allocating a location for.
	 * @param context
	 */
	public Location[] allocateLocations(Context context, Expr... es) {
		Location[] rs = new Location[es.length];
		int offset = 0;
		for (int i = 0; i != rs.length; ++i) {
			Expr e = es[i];
			Type type = unwrap(e.attribute(Attribute.Type.class).type);
			if (type instanceof Type.Record) {
				rs[i] = new MemoryLocation(type, HSP, offset);
				offset += determineWidth(type);
			} else {
				RegisterLocation reg = context.selectFreeRegister(type);
				// Temporarily lock the register so that we don't allocate it
				// again!
				context = context.lockLocation(reg);
				rs[i] = reg;
			}
		}
		allocateSpaceOnStack(offset, context);
		return rs;
	}

	/**
	 * Release any space allocated for this particular location. If the location
	 * was allocated to a register, then this will do nothing. However, if the
	 * location was allocated to some space on the stack then it will release
	 * this space by moving the stack pointer accordingly.
	 * 
	 * @param l
	 * @param context
	 */
	public void freeLocations(Context context, Location... ls) {
		int stackToFree = 0;
		for (int i = 0; i != ls.length; ++i) {
			Location l = ls[i];
			if (l instanceof MemoryLocation) {
				stackToFree += determineWidth(l.type);
			} else {
				// nothing to free here
			}
		}
		freeSpaceOnStack(stackToFree, context);
	}

	/**
	 * Perform a bitwise copy from one location to another. If both locations
	 * are registers, then it's just a register assignment. Otherwise, it will
	 * involve one or more indirect reads / writes.
	 * 
	 * @param from
	 *            Location to copy bits from
	 * @param to
	 *            Location to copy bits to
	 */
	public void bitwiseCopy(Location from, Location to, Context context) {
		List<Instruction> instructions = context.instructions();
		// Make a quick sanity check
		if (from.equals(to)) {
			// In this very special case, we are attempting to copy to/from the
			// same location. Hence, we can just do nothing.
			return;
		}
		if (from instanceof RegisterLocation && to instanceof RegisterLocation) {
			// direct register-to-register copy
			Register fromReg = ((RegisterLocation) from).register;
			Register toReg = ((RegisterLocation) to).register;
			instructions.add(new Instruction.RegReg(Instruction.RegRegOp.mov, fromReg, toReg));
		} else if (from instanceof RegisterLocation) {
			// register-to-memory copy
			Register fromReg = ((RegisterLocation) from).register;
			MemoryLocation toLoc = (MemoryLocation) to;
			instructions.add(new Instruction.RegImmInd(Instruction.RegImmIndOp.mov, fromReg, toLoc.offset, toLoc.base));
		} else if (to instanceof RegisterLocation) {
			// memory-to-register copy
			MemoryLocation fromLoc = (MemoryLocation) from;
			Register toReg = ((RegisterLocation) to).register;
			instructions
					.add(new Instruction.ImmIndReg(Instruction.ImmIndRegOp.mov, fromLoc.offset, fromLoc.base, toReg));
		} else {
			MemoryLocation fromLoc = (MemoryLocation) from;
			MemoryLocation toLoc = (MemoryLocation) to;
			int width = determineWidth(fromLoc.type());
			int oneWordWidth = target.widthInBytes();
			RegisterLocation tmp = context.selectFreeRegister(to.type());
			for (int i = 0; i < width; i = i + oneWordWidth) {
				instructions.add(new Instruction.ImmIndReg(Instruction.ImmIndRegOp.mov, fromLoc.offset + i,
						fromLoc.base, tmp.register));
				instructions.add(new Instruction.RegImmInd(Instruction.RegImmIndOp.mov, tmp.register, toLoc.offset + i,
						toLoc.base));
			}
		}
	}

	/**
	 * Perform bitwise equality test of two locations. If they are not equal,
	 * then branch to the false label. Otherwise, control proceeds to the next
	 * instruction in sequence.
	 * 
	 * @param lhs
	 * @param rhs
	 * @param falseLabel
	 */
	public void bitwiseEquality(Location lhs, Location rhs, String falseLabel, Context context) {
		List<Instruction> instructions = context.instructions();
		// Make a quick sanity check
		if (lhs.equals(rhs)) {
			// In this very special case, we are attempting to copy to/from the
			// same location. Hence, we can just do nothing.
			return;
		}
		if (lhs instanceof RegisterLocation && rhs instanceof RegisterLocation) {
			// direct register-to-register copy
			Register lhsReg = ((RegisterLocation) lhs).register;
			Register rhsReg = ((RegisterLocation) rhs).register;
			instructions.add(new Instruction.RegReg(Instruction.RegRegOp.cmp, lhsReg, rhsReg));
			instructions.add(new Instruction.Addr(Instruction.AddrOp.jnz, falseLabel));
		} else if (lhs instanceof RegisterLocation) {
			// register-to-memory copy
			Register lhsReg = ((RegisterLocation) lhs).register;
			RegisterLocation tmp = context.selectFreeRegister(rhs.type());
			MemoryLocation rhsLoc = (MemoryLocation) rhs;
			instructions.add(
					new Instruction.ImmIndReg(Instruction.ImmIndRegOp.mov, rhsLoc.offset, rhsLoc.base, tmp.register));
			instructions.add(new Instruction.RegReg(Instruction.RegRegOp.cmp, lhsReg, tmp.register));
			instructions.add(new Instruction.Addr(Instruction.AddrOp.jnz, falseLabel));
		} else if (rhs instanceof RegisterLocation) {
			// memory-to-register copy
			RegisterLocation tmp = context.selectFreeRegister(lhs.type());
			MemoryLocation lhsLoc = (MemoryLocation) lhs;
			Register rhsReg = ((RegisterLocation) rhs).register;
			instructions.add(
					new Instruction.ImmIndReg(Instruction.ImmIndRegOp.mov, lhsLoc.offset, lhsLoc.base, tmp.register));
			instructions.add(new Instruction.RegReg(Instruction.RegRegOp.cmp, rhsReg, tmp.register));
			instructions.add(new Instruction.Addr(Instruction.AddrOp.jnz, falseLabel));
		} else {
			// memory-to-memory copy of arbitrary width
			MemoryLocation lhsLoc = (MemoryLocation) lhs;
			MemoryLocation rhsLoc = (MemoryLocation) rhs;
			RegisterLocation lhsReg = context.selectFreeRegister(lhsLoc.type());
			context = context.lockLocation(lhsReg);
			RegisterLocation rhsReg = context.selectFreeRegister(rhsLoc.type());
			int width = determineWidth(lhsLoc.type());
			int oneWordWidth = target.widthInBytes();
			for (int i = 0; i < width; i = i + oneWordWidth) {
				instructions.add(new Instruction.ImmIndReg(Instruction.ImmIndRegOp.mov, lhsLoc.offset + i, lhsLoc.base,
						lhsReg.register));
				instructions.add(new Instruction.ImmIndReg(Instruction.ImmIndRegOp.mov, rhsLoc.offset + i, rhsLoc.base,
						rhsReg.register));
				instructions.add(new Instruction.RegReg(Instruction.RegRegOp.cmp, rhsReg.register, lhsReg.register));
				instructions.add(new Instruction.Addr(Instruction.AddrOp.jnz, falseLabel));
			}
		}
	}

	/**
	 * <p>
	 * Allocate a chunk of space on the stack. The stack pointer will be moved
	 * accordingly passed the allocated space. This may allocate more space that
	 * actually required. In particular,padding may have been allocated by the
	 * caller to ensure the stack was aligned to a 16byte boundary on entry to
	 * the function.
	 * </p>
	 *
	 * <p>
	 * <b>NOTE:</b>The use of padding is not strictly necessary for correct
	 * operation. However, when calling external functions (e.g. malloc, assert,
	 * printInt) we must adhere to the System V ABI rules which dictate this
	 * alignment. Therefore, it's much easier if we just keep the stack aligned
	 * the whole way along.
	 * </p>
	 * 
	 * @param minimumWidth
	 *            The number of bytes to allocate on the stack. This is a
	 *            minimum value, as more bytes might actually be allocated than
	 *            requested (i.e. for padding).
	 * @param code
	 *            Code section to add on those instructions corresponding to
	 *            this expression
	 */
	private int allocateSpaceOnStack(int minimumWidth, Context context) {
		if (minimumWidth > 0) {
			List<Instruction> instructions = context.instructions();
			int paddedWidth = determinePaddedWidth(minimumWidth);
			instructions.add(new Instruction.ImmReg(Instruction.ImmRegOp.sub, paddedWidth, HSP));
			return paddedWidth;
		} else {
			return 0;
		}
	}

	/**
	 * <p>
	 * Free a chunk of space allocated on the stack. The stack pointer will be
	 * moved accordingly passed the allocated space. This may allocate more
	 * space that actually required. In particular,padding may have been
	 * allocated by the caller to ensure the stack was aligned to a 16byte
	 * boundary on entry to the function.
	 * </p>
	 *
	 * <p>
	 * <b>NOTE:</b>The use of padding is not strictly necessary for correct
	 * operation. However, when calling external functions (e.g. malloc, assert,
	 * printInt) we must adhere to the System V ABI rules which dictate this
	 * alignment. Therefore, it's much easier if we just keep the stack aligned
	 * the whole way along.
	 * </p>
	 * 
	 * @param minimumWidth
	 *            The number of bytes to allocate on the stack. This is a
	 *            minimum value, as more bytes might actually be allocated than
	 *            requested (i.e. for padding).
	 * @param code
	 *            Code section to add on those instructions corresponding to
	 *            this expression
	 */
	private void freeSpaceOnStack(int minimumWidth, Context context) {
		if (minimumWidth > 0) {
			List<Instruction> instructions = context.instructions();
			int paddedWidth = determinePaddedWidth(minimumWidth);
			instructions.add(new Instruction.ImmReg(Instruction.ImmRegOp.add, paddedWidth, HSP));
		}
	}

	/**
	 * <p>
	 * Allocate memory on the heap dynamically using malloc. This expects the
	 * amount of space to be allocated is held in the target register. On exit,
	 * the target register will hold a pointer to the memory in question. This
	 * method will ensure that all other registers are saved properly.
	 * </p>
	 * <p>
	 * <b>NOTE:</b> this does not allocate any additional padding, as this is
	 * unnecessary for heap-allocated data.
	 * </p>
	 *
	 * @param target
	 *            This register must contain size (in bytes) to be allocated
	 *            and, on return, will hold a pointer to the heap allocated
	 *            data.
	 */
	public void allocateSpaceOnHeap(Register target, Context context) {
		makeExternalMethodCall("malloc", context, target, target);
	}

	/**
	 * Determine the width of the callee stack frame. That is, the amount of
	 * space which must be reserved for the local variables.
	 * 
	 * @param method
	 * @return
	 */
	private int determineCalleeStackFrameWidth(WhileFile.MethodDecl method) {
		HashMap<String, Type> variables = new HashMap<String, Type>();
		extractLocalVariableTypes(method.getBody(), variables);
		// The starting offset for local variables must be below the old frame
		// pointer (which is one natural word width).
		int count = 0;
		// Now, allocate each variable by descending from current offset.
		for (Map.Entry<String, Type> e : variables.entrySet()) {
			int width = determineWidth(e.getValue());
			count += width;
		}
		return count;
	}

	/**
	 * Determine the amount of space required for the parameters and return
	 * value of a given method. This must be padded out so that it is aligned on
	 * certain architectures.
	 * 
	 * @param md
	 * @return
	 */
	public int determineCallerEnvironmentWidth(WhileFile.MethodDecl md) {
		int width = 0;
		for (WhileFile.Parameter p : md.getParameters()) {
			width += determineWidth(p.getType());
		}
		width += determineWidth(md.getRet());
		return width;
	}

	/**
	 * This ensures that the width returned is a multiple of 16. This is
	 * necessary to ensure that the stack is 16byte aligned in order to meet the
	 * requirements of the System V ABI.
	 * 
	 * @param minimum
	 *            The minumum number of bytes required for the stack frame to
	 *            hold all the necessary local variables, etc.
	 * @return
	 */
	private int determinePaddedWidth(int minimum) {
		// round up to nearest 16 bytes
		int tmp = (minimum / 16) * 16;
		if (tmp < minimum) {
			tmp = tmp + 16;
		}
		return tmp;
	}

	/**
	 * Determine the width (in bytes) of this type. For simplicity we round
	 * everything to the nearest "natural" word size for the given architecture.
	 * For example, on x86_64, this function returns 8 for type bool. Obviously,
	 * this is not the most efficient.
	 * 
	 * @param type
	 * @return
	 */
	public int determineWidth(Type type) {
		if (type instanceof Type.Void) {
			return 0;
		} else if (type instanceof Type.Bool || type instanceof Type.Char || type instanceof Type.Int) {
			// The size of a machine word.
			return target.widthInBytes();
		} else if (type instanceof Type.Record) {
			Type.Record r = (Type.Record) type;
			int total = 0;
			for (Pair<Type, String> e : r.getFields()) {
				total += determineWidth(e.first());
			}
			return total;
		} else if (type instanceof Type.Strung) {
			// Always the size of a machine pointer.
			return target.widthInBytes();
		} else if (type instanceof Type.Named) {
			return determineWidth(unwrap(type));
		} else {
			throw new IllegalArgumentException("Unknown type encountered: " + type);
		}
	}

	/**
	 * Determine the offset of a given field in a given type. This is done by
	 * calculating the width of each field until we find the one we're looking
	 * for.
	 * 
	 * @param type
	 *            --- Type of the record containing the field whose offset we
	 *            wish to calculate.
	 * @param field
	 *            --- Name of the field whose offset we wish to calculate.
	 */
	public int getFieldOffset(Type.Record type, String field) {
		// Calculate offset of field we are reading
		int offset = 0;

		for (Pair<Type, String> e : type.getFields()) {
			if (e.second().equals(field)) {
				break;
			}
			offset += determineWidth(e.first());
		}

		return offset;
	}

	/**
	 * Determine the type of a given field
	 * 
	 * @param type
	 * @param field
	 * @return
	 */
	public Type getFieldType(Type.Record type, String field) {
		for (Pair<Type, String> e : type.getFields()) {
			if (e.second().equals(field)) {
				return e.first();
			}
		}
		throw new IllegalArgumentException("invalid field: " + field);
	}

	/**
	 * Determine the type of every declared local variable. In cases where we
	 * have two local variables with the same name but different types, choose
	 * the physically largest type (in bytes).
	 * 
	 * @param statements
	 * @param allocation
	 */
	private void extractLocalVariableTypes(List<Stmt> statements, Map<String, Type> allocation) {
		for (Stmt stmt : statements) {
			if (stmt instanceof Stmt.VariableDeclaration) {
				Stmt.VariableDeclaration vd = (Stmt.VariableDeclaration) stmt;
				Type ot = allocation.get(vd.getName());
				Type nt = vd.getType();
				if (ot != null && determineWidth(ot) > determineWidth(nt)) {
					nt = ot;
				}
				allocation.put(vd.getName(), nt);
			} else if (stmt instanceof Stmt.IfElse) {
				Stmt.IfElse ife = (Stmt.IfElse) stmt;
				extractLocalVariableTypes(ife.getTrueBranch(), allocation);
				extractLocalVariableTypes(ife.getFalseBranch(), allocation);
			} else if (stmt instanceof Stmt.For) {
				Stmt.For fe = (Stmt.For) stmt;

				// Allocate loop variable
				Stmt.VariableDeclaration vd = fe.getDeclaration();
				Type ot = allocation.get(vd.getName());
				Type nt = vd.getType();
				if (ot != null && determineWidth(ot) > determineWidth(nt)) {
					nt = ot;
				}
				allocation.put(vd.getName(), nt);
				// Explore loop body
				extractLocalVariableTypes(fe.getBody(), allocation);
			} else if (stmt instanceof Stmt.While) {
				Stmt.While fe = (Stmt.While) stmt;
				extractLocalVariableTypes(fe.getBody(), allocation);
			} else if (stmt instanceof Stmt.Switch) {
				Stmt.Switch fe = (Stmt.Switch) stmt;
				for (Stmt.Case c : fe.getCases()) {
					extractLocalVariableTypes(c.getBody(), allocation);
				}
			}
		}
	}

	public Type unwrap(Type type) {
		if (type instanceof Type.Named) {
			Type.Named tn = (Type.Named) type;
			WhileFile.TypeDecl td = types.get(tn.getName());
			return td.getType();
		} else {
			return type;
		}
	}

	public boolean isIntArray(Type t) {
		return t instanceof Type.Array && ((Type.Array) t).getElement() instanceof Type.Int;
	}

	private static int labelIndex = 0;

	public static String freshLabel() {
		return "label" + labelIndex++;
	}

	/**
	 * Determine the appropriate symbol name to use when calling a method which
	 * employs standard C calling conventions. Perhaps surprisingly, this is
	 * architecture dependent. In particular, MacOS usese the Mach-O object file
	 * format and supports alternative calling conventions (perhaps for
	 * efficiency?). Thus, on MacOS, symbols which adhere to standard calling
	 * conventions are prefixed with an underscore.
	 * 
	 * @param name
	 */
	private String externalSymbolName(String name) {
		if (target == Target.MACOS_X86_64) {
			return "_" + name;
		} else {
			return name;
		}
	}

	/**
	 * Add a standard main method which will be called by the operating system
	 * when this process is executed. This sequence is operating system
	 * dependent, and simply calls the translated <code>main()</code> method
	 * from the original while source file.
	 * 
	 * @param xf
	 */
	private void addMainLauncher(X86File.Code code) {
		List<Instruction> instructions = code.instructions;
		instructions.add(new Instruction.Label(externalSymbolName("main"), 1, true));
		instructions.add(new Instruction.Reg(Instruction.RegOp.push, HBP));
		instructions.add(new Instruction.Addr(Instruction.AddrOp.call, "wl_main"));
		instructions.add(new Instruction.Reg(Instruction.RegOp.pop, HBP));
		// Ensure an exit code of 0 if we get here. If not, then an assertion
		// occurred and we get a non-zero exit code.
		instructions.add(new Instruction.ImmReg(Instruction.ImmRegOp.mov, 0, HAX));
		instructions.add(new Instruction.Unit(Instruction.UnitOp.ret));
	}

	/**
	 * Returns the head of a given registers family. For example, on
	 * <code>x86_64</code> the head of the <code>bx</code> family is
	 * <code>rbx</code>. Conversely, the head of the <code>bx</code> family is
	 * <code>ebx</code> on <code>x86_32</code>.
	 * 
	 * @param register
	 * @return
	 */
	private Register headOfFamily(Register register) {
		Register.Width width;
		switch (target.arch) {
		case X86_32:
			width = Register.Width.Long;
			break;
		case X86_64:
			width = Register.Width.Quad;
			break;
		default:
			throw new IllegalArgumentException("Invalid architecture: " + target.arch);
		}
		return register.sibling(width);
	}

	private final Register HAX;
	private final Register HBX;
	private final Register HCX;
	private final Register HDX;
	private final Register HDI;
	private final Register HSI;
	private final Register HBP;
	private final Register HSP;
	private final Register HIP;

	public final List<Register> REGISTER_POOL;

	/**
	 * An abstraction describing the destination for the result of a given
	 * expression. For results which are guaranteed to fit into a register, we
	 * want a register target. For results which don't fit into a register (e.g.
	 * a record) we need a stack target.
	 * 
	 * @author David J. Pearce
	 *
	 */
	private abstract class Location {
		private Type type; // unwrapped type

		public Location(Type t) {
			this.type = t;
		}

		public Type type() {
			return type;
		}
	}

	/**
	 * A register target is the simple case where the given result fits into a
	 * register.
	 * 
	 * @author David J. Pearce
	 *
	 */
	private class RegisterLocation extends Location {
		private final Register register;

		public RegisterLocation(Type type, Register target) {
			super(type);
			this.register = target;
		}

		@Override
		public boolean equals(Object o) {
			if (o instanceof RegisterLocation) {
				RegisterLocation l = (RegisterLocation) o;
				return l.register == register;
			}
			return false;
		}

		@Override
		public int hashCode() {
			return register.hashCode();
		}

		public String toString() {
			return register.toString();
		}
	}

	/**
	 * Describes a location in memory relative to a given base register
	 * (typically either the stack or frame pointer). This is necessary for the
	 * case where we are writing a value which doesn't fit into a single
	 * register (e.g. a record).
	 * 
	 * @author David J. Pearce
	 *
	 */
	private class MemoryLocation extends Location {
		private final Register base;
		private final int offset;

		public MemoryLocation(Type type, Register base, int offset) {
			super(type);
			this.base = base;
			this.offset = offset;
		}

		@Override
		public boolean equals(Object o) {
			if (o instanceof MemoryLocation) {
				MemoryLocation l = (MemoryLocation) o;
				return base == l.base && offset == l.offset;
			}
			return false;
		}

		@Override
		public int hashCode() {
			return base.hashCode() ^ offset;
		}

		public String toString() {
			return "&(" + base + "+" + offset + ")";
		}
	}

	private class Context {
		private final Map<String, MemoryLocation> localVariables;
		private final String exitLabel;
		private final X86File.Code code;
		private final X86File.Data data;
		private final List<Register> freeRegisters;

		public Context(List<Register> freeRegisters, Map<String, MemoryLocation> localVariables, String exitLabel,
				X86File.Code code, X86File.Data data) {
			this.localVariables = localVariables;
			this.code = code;
			this.data = data;
			this.freeRegisters = freeRegisters;
			this.exitLabel = exitLabel;
		}

		public MemoryLocation getVariableLocation(String name) {
			return localVariables.get(name);
		}

		public String exitLabel() {
			return exitLabel;
		}

		public List<Instruction> instructions() {
			return code.instructions;
		}

		public List<Constant> constants() {
			return data.constants;
		}

		/**
		 * Select a free register from the list of free registers. Note that
		 * this register is not removed from the list of free registers (as this
		 * only happens when the register is locked).
		 * 
		 * @param freeRegister
		 * @return
		 */
		private RegisterLocation selectFreeRegister(Type type) {
			// FIXME: The following line fails because it does not handle the
			// case where we run out of registers. In such case, we need to
			// implement some kind of register spilling mechanism.
			Register reg = freeRegisters.get(0);
			return new RegisterLocation(type, reg);
		}

		/**
		 * "lock" a given location. This only has an effect if the location is a
		 * register, in which case it is removed from the list of free
		 * registers. We are essentially "locking" that register and preventing
		 * it from being used as a location for a subsequent operation.
		 * 
		 * @param freeRegister.
		 *            The free register to lock. This should be in the list of
		 *            free registers.
		 */
		private Context lockLocation(Location location) {
			if (location instanceof RegisterLocation) {
				RegisterLocation sl = (RegisterLocation) location;
				// Quick sanity check
				if (!freeRegisters.contains(sl.register)) {
					throw new IllegalArgumentException("attempting to lock register which is already locked");
				}
				ArrayList<Register> nFreeRegs = new ArrayList<Register>(freeRegisters);
				nFreeRegs.remove(sl.register);
				return new Context(nFreeRegs, localVariables, exitLabel, code, data);
			} else {
				return this;
			}
		}

		/**
		 * "unlock" a given location. This only has an effect if the location is
		 * a register, in which case it is removed from the list of free
		 * registers. We are essentially "unlocking" that register so that it
		 * can be used as a location for a subsequent operation.
		 * 
		 * @param freeRegister.
		 *            The free register to unlock. This should be in the list of
		 *            free registers.
		 */
		private Context unlockLocation(Location location) {
			if (location instanceof RegisterLocation) {
				RegisterLocation sl = (RegisterLocation) location;
				// First, do a sanity check
				if (freeRegisters.contains(sl.register)) {
					throw new IllegalArgumentException("attempting to unlock register which is not locked");
				}
				ArrayList<Register> nFreeRegs = new ArrayList<Register>(freeRegisters);
				nFreeRegs.add(sl.register);
				return new Context(nFreeRegs, localVariables, exitLabel, code, data);
			} else {
				return this;
			}
		}

		/**
		 * Determine the set of used registers from a given pool of registers.
		 * That is, the registers in the pool which are not in the list of free
		 * registers.
		 * 
		 * @param freeRegisters
		 * @return
		 */
		public List<Register> getUsedRegisters(List<Register> pool) {
			ArrayList<Register> usedRegisters = new ArrayList<Register>();
			for (int i = 0; i != pool.size(); ++i) {
				Register r = pool.get(i);
				if (!freeRegisters.contains(r)) {
					usedRegisters.add(r);
				}
			}
			return usedRegisters;
		}

	}
}
