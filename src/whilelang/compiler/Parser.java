// This file is part of the WhileLang Compiler (wlc).
//
// The WhileLang Compiler is free software; you can redistribute
// it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// The WhileLang Compiler is distributed in the hope that it
// will be useful, but WITHOUT ANY WARRANTY; without even the
// implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public
// License along with the WhileLang Compiler. If not, see
// <http://www.gnu.org/licenses/>
//
// Copyright 2013, David James Pearce.

package whilelang.compiler;

import java.io.File;
import java.util.*;

import whilelang.ast.Attribute;
import whilelang.ast.Expr;
import whilelang.ast.Stmt;
import whilelang.ast.Type;
import whilelang.ast.WhileFile;
import whilelang.ast.WhileFile.*;
import whilelang.compiler.Lexer.*;
import whilelang.util.Pair;
import whilelang.util.SyntaxError;

public class Parser {

	private String filename;
	private ArrayList<Token> tokens;
	private HashSet<String> userDefinedTypes;
	private int index;

	public Parser(String filename, List<Token> tokens) {
		this.filename = filename;
		this.tokens = new ArrayList<Token>(tokens);
		this.userDefinedTypes = new HashSet<String>();
	}

	/**
	 * Parse a given source file to produce its Abstract Syntax Tree
	 * representation.
	 * 
	 * @return
	 */
	public WhileFile read() {
		ArrayList<Decl> decls = new ArrayList<Decl>();

		while (index < tokens.size()) {
			Token t = tokens.get(index);
			if (t instanceof Keyword) {
				Keyword k = (Keyword) t;
				if (t.text.equals("type")) {
					decls.add(parseTypeDeclaration());
				} else {
					decls.add(parseMethodDeclaration());
				}
			} else {
				decls.add(parseMethodDeclaration());
			}
		}

		return new WhileFile(filename, decls);
	}

	/**
	 * Parse a type declaration of the following form:
	 * 
	 * <pre>
	 * TypeDecl ::= 'type' Ident 'is' Type
	 * </pre>
	 * 
	 * @return
	 */
	private Decl parseTypeDeclaration() {
		int start = index;
		matchKeyword("type");

		Identifier name = matchIdentifier();

		matchKeyword("is");

		Type t = parseType();
		int end = index;
		userDefinedTypes.add(name.text);
		return new TypeDecl(t, name.text, sourceAttr(start, end - 1));
	}

	/**
	 * Parse a method declaration, of the form:
	 * 
	 * <pre>
	 * MethodDecl ::= Type Ident '(' Parameters ')' '{' Stmt* '}'
	 * 
	 * Parameters ::= [Type Ident (',' Type Ident)* ]
	 * </pre>
	 * 
	 * @return
	 */
	private WhileFile.MethodDecl parseMethodDeclaration() {
		int start = index;

		Type returnType = parseType();
		Identifier name = matchIdentifier();

		match("(");

		// Now build up the parameter types
		List<Parameter> paramTypes = new ArrayList<Parameter>();
		boolean firstTime = true;
		while (index < tokens.size() && !(tokens.get(index) instanceof RightBrace)) {
			if (!firstTime) {
				match(",");
			}
			firstTime = false;
			int parameterStart = index;
			Type parameterType = parseType();
			Identifier parameterName = matchIdentifier();
			paramTypes.add(new Parameter(parameterType, parameterName.text, sourceAttr(parameterStart, index - 1)));
		}

		match(")");
		List<Stmt> stmts = parseStatementBlock();
		return new WhileFile.MethodDecl(name.text, returnType, paramTypes, stmts, sourceAttr(start, index - 1));
	}

	/**
	 * Parse a block of zero or more statements, of the form:
	 * 
	 * <pre>
	 * StmtBlock ::= '{' Stmt* '}'
	 * </pre>
	 * 
	 * @return
	 */
	private List<Stmt> parseStatementBlock() {
		match("{");

		ArrayList<Stmt> stmts = new ArrayList<Stmt>();
		while (index < tokens.size() && !(tokens.get(index) instanceof RightCurly)) {
			stmts.add(parseStatement(true));
		}

		match("}");

		return stmts;
	}

	/**
	 * Parse a given statement.
	 * 
	 * @param withSemiColon
	 *            Indicates whether to match semi-colons after the statement
	 *            (where appropriate). This is useful as in some very special
	 *            cases (e.g. for-loops) we don't want to match semi-colons.
	 * @return
	 */
	private Stmt parseStatement(boolean withSemiColon) {
		checkNotEof();
		Token token = tokens.get(index);
		Stmt stmt;
		if (token.text.equals("assert")) {
			stmt = parseAssertStmt();
			if (withSemiColon) {
				match(";");
			}
		} else if (token.text.equals("return")) {
			stmt = parseReturnStmt();
			if (withSemiColon) {
				match(";");
			}
		} else if (token.text.equals("print")) {
			stmt = parsePrintStmt();
			if (withSemiColon) {
				match(";");
			}
		} else if (token.text.equals("if")) {
			stmt = parseIfStmt();
		} else if (token.text.equals("while")) {
			stmt = parseWhileStmt();
		} else if (token.text.equals("for")) {
			stmt = parseForStmt();
		} else if (token.text.equals("switch")) {
			stmt = parseSwitchStmt();
		} else if (token.text.equals("break")) {
			stmt = parseBreakStmt();
			if (withSemiColon) { match(";"); }
		} else if (token.text.equals("continue")) {
			stmt = parseContinueStmt();
			if (withSemiColon) { match(";"); }
		} else if ((index + 1) < tokens.size() && tokens.get(index + 1) instanceof LeftBrace) {
			// must be a method invocation
			stmt = parseInvokeExprOrStmt();
			if (withSemiColon) {
				match(";");
			}
		} else if (isTypeAhead(index)) {
			stmt = parseVariableDeclaration();
			if (withSemiColon) {
				match(";");
			}
		} else {
			// invocation or assignment
			int start = index;
			Expr t = parseExpr();
			if (t instanceof Expr.Invoke) {
				stmt = (Expr.Invoke) t;
			} else {
				index = start;
				stmt = parseAssignStmt();
			}
			if (withSemiColon) {
				match(";");
			}
		}
		return stmt;
	}

	/**
	 * Check whether there is a type starting at the given index. This is useful
	 * for distinguishing variable declarations from invocations and
	 * assignments.
	 * 
	 * @param index
	 * @return
	 */
	private boolean isTypeAhead(int index) {
		if (index >= tokens.size()) {
			return false;
		}
		Token lookahead = tokens.get(index);
		if (lookahead instanceof Keyword) {
			return lookahead.text.equals("null") || lookahead.text.equals("bool") || lookahead.text.equals("int")
					|| lookahead.text.equals("char") || lookahead.text.equals("string");
		} else if (lookahead instanceof Identifier) {
			Identifier id = (Identifier) lookahead;
			return userDefinedTypes.contains(id.text);
		} else if (lookahead instanceof LeftCurly) {
			return isTypeAhead(index + 1);
		} else if (lookahead instanceof LeftSquare) {
			return isTypeAhead(index + 1);
		}

		return false;
	}

	/**
	 * Parse an assert statement, of the form:
	 * 
	 * <pre>
	 * AssertStmt ::= 'assert'  Expr ';'
	 * </pre>
	 * 
	 * @return
	 */
	private Stmt.Assert parseAssertStmt() {
		int start = index;
		// Every return statement begins with the return keyword!
		matchKeyword("assert");
		Expr e = parseExpr();
		// Done.
		return new Stmt.Assert(e, sourceAttr(start, index - 1));
	}

	/**
	 * Parse an assignment statement, of the form:
	 * 
	 * <pre>
	 * AssignStmt ::= LVal '=' Expr ';'
	 * 
	 * LVal ::= Ident
	 *       | LVal '.' Ident
	 *       | LVal '[' Expr ']'
	 * </pre>
	 * 
	 * @return
	 */
	private Stmt parseAssignStmt() {
		// standard assignment
		int start = index;
		Expr lhs = parseExpr();
		if (!(lhs instanceof Expr.LVal)) {
			syntaxError("expecting lval, found " + lhs + ".", lhs);
		}
		match("=");
		Expr rhs = parseExpr();
		int end = index;
		return new Stmt.Assign((Expr.LVal) lhs, rhs, sourceAttr(start, end - 1));
	}

	/**
	 * Parse a print statement, of the form:
	 * 
	 * <pre>
	 * PrintStmt ::= 'print' Expr ';'
	 * </pre>
	 * 
	 * @return
	 */
	private Stmt.Print parsePrintStmt() {
		int start = index;
		matchKeyword("print");
		checkNotEof();
		Expr e = parseExpr();
		int end = index;
		return new Stmt.Print(e, sourceAttr(start, end - 1));
	}

	/**
	 * Parse a variable declaration, of the form:
	 * 
	 * <pre>
	 * VarDecl ::= Type Ident [ '=' Expr ] ';'
	 * </pre>
	 * 
	 * @return
	 */
	private Stmt.VariableDeclaration parseVariableDeclaration() {
		int start = index;
		// Every variable declaration consists of a declared type and variable
		// name.
		Type type = parseType();
		Identifier id = matchIdentifier();
		// A variable declaration may optionally be assigned an initialiser
		// expression.
		Expr initialiser = null;
		if (index < tokens.size() && tokens.get(index) instanceof Equals) {
			match("=");
			initialiser = parseExpr();
		}
		// Done.
		return new Stmt.VariableDeclaration(type, id.text, initialiser, sourceAttr(start, index - 1));
	}

	/**
	 * Parse an if statement, of the form:
	 * 
	 * <pre>
	 * IfStmt ::= 'if' '(' Expr ')' StmtBlock ElseIf* [Else]
	 * 
	 * ElseIf ::= 'else' 'if' '(' Expr ')' StmtBlock
	 * 
	 * Else ::= 'else' StmtBlock
	 * </pre>
	 * 
	 * @return
	 */
	private Stmt parseIfStmt() {
		int start = index;
		matchKeyword("if");
		match("(");
		Expr c = parseExpr();
		match(")");
		int end = index;
		List<Stmt> tblk = parseStatementBlock();
		List<Stmt> fblk = Collections.emptyList();

		if ((index + 1) < tokens.size() && tokens.get(index).text.equals("else")) {
			matchKeyword("else");

			if (index < tokens.size() && tokens.get(index).text.equals("if")) {
				Stmt if2 = parseIfStmt();
				fblk = new ArrayList<Stmt>();
				fblk.add(if2);
			} else {
				fblk = parseStatementBlock();
			}
		}

		return new Stmt.IfElse(c, tblk, fblk, sourceAttr(start, end - 1));
	}

	/**
	 * Parse a return statement, of the form:
	 * 
	 * <pre>
	 * ReturnStmt ::= 'return' [ Expr ] ';'
	 * </pre>
	 * 
	 * @return
	 */
	private Stmt.Return parseReturnStmt() {
		int start = index;
		// Every return statement begins with the return keyword!
		matchKeyword("return");
		Expr e = null;
		// A return statement may optionally have a return expression.
		if (index < tokens.size() && !(tokens.get(index) instanceof SemiColon)) {
			e = parseExpr();
		}
		// Done.
		return new Stmt.Return(e, sourceAttr(start, index - 1));
	}

	/**
	 * Parse a While statement of the form:
	 * 
	 * <pre>
	 * WhileStmt ::= 'while' '(' Expr ')' StmtBlock
	 * </pre>
	 * 
	 * @return
	 */
	private Stmt parseWhileStmt() {
		int start = index;
		matchKeyword("while");
		match("(");
		Expr condition = parseExpr();
		match(")");
		int end = index;
		List<Stmt> blk = parseStatementBlock();
		return new Stmt.While(condition, blk, sourceAttr(start, end - 1));
	}

	/**
	 * Parse a for statement, of the form:
	 * 
	 * <pre>
	 * ForStmt ::=
	 * </pre>
	 * 
	 * @return
	 */
	private Stmt parseForStmt() {
		int start = index;
		matchKeyword("for");
		match("(");
		Stmt.VariableDeclaration declaration = parseVariableDeclaration();
		match(";");
		Expr condition = parseExpr();
		match(";");
		Stmt increment = parseStatement(false);
		int end = index;
		match(")");
		List<Stmt> blk = parseStatementBlock();

		return new Stmt.For(declaration, condition, increment, blk, sourceAttr(start, end - 1));
	}

	/**
	 * Parse a Switch statement of the form:
	 * 
	 * <pre>
	 * SwitchStmt ::= 'switch' '(' Expr ')' '{' CaseBlock+ [DefaultBlock] '}'
	 * 
	 * CaseBlock ::= 'case' Expr ':' Stmt*
	 * 
	 * DefaultBlock ::= 'default' ':' Stmt*
	 * </pre>
	 * 
	 * @return
	 */
	private Stmt parseSwitchStmt() {
		int start = index;
		matchKeyword("switch");
		match("(");
		Expr expr = parseExpr();
		match(")");
		int end = index;
		match("{");
		List<Stmt.Case> cases = parseSwitchCases();
		match("}");

		return new Stmt.Switch(expr, cases, sourceAttr(start, end - 1));
	}

	private Stmt parseBreakStmt() {
		int start = index;
		matchKeyword("break");
		return new Stmt.Break(sourceAttr(start, index - 1));
	}
	
	private Stmt parseContinueStmt() {
		int start = index;
		matchKeyword("continue");
		return new Stmt.Continue(sourceAttr(start, index - 1));
	}
	/**
	 * Parse the list of zero or more case blocks which make up a switch
	 * statement.
	 * 
	 * @return
	 */
	private List<Stmt.Case> parseSwitchCases() {
		ArrayList<Stmt.Case> cases = new ArrayList<Stmt.Case>(); 
		while(index < tokens.size() && !(tokens.get(index) instanceof RightCurly)) {
			int start = index;
			Expr.Constant value;
			Token lookahead = tokens.get(index);			
			if(lookahead.text.equals("case")) {
				// This is a case block
				matchKeyword("case");
				value = parseConstant();
				match(":");
			} else {
				// This must be a default block
				matchKeyword("default");
				match(":");
				value = null;
			} 
			int end = index;
			// Parse the case body
			ArrayList<Stmt> body = new ArrayList<Stmt>();
			while (index < tokens.size() && !(tokens.get(index) instanceof RightCurly)
					&& !(tokens.get(index).text.equals("case")) && !(tokens.get(index).text.equals("default"))) {
				body.add(parseStatement(true));
			}
			cases.add(new Stmt.Case(value, body, sourceAttr(start,end-1)));
		}
		return cases; 
	}

	private Expr.Constant parseConstant() {
		Expr e = parseExpr();
		Object constant = parseConstant(e);
		return new Expr.Constant(constant,e.attributes());
	}
	
	private Object parseConstant(Expr e) {
		if(e instanceof Expr.Constant) {
			return ((Expr.Constant) e).getValue();
		} else if(e instanceof Expr.ArrayInitialiser) {
			Expr.ArrayInitialiser ai = (Expr.ArrayInitialiser) e;
			ArrayList<Object> vals = new ArrayList<Object>();
			for(Expr element : ai.getArguments()) {
				vals.add(parseConstant(element));
			}
			return vals;			
		} else if(e instanceof Expr.RecordConstructor) {
			Expr.RecordConstructor rc = (Expr.RecordConstructor) e;
			HashMap<String,Object> vals = new HashMap<String,Object>();
			for(Pair<String,Expr> p : rc.getFields()) {
				vals.put(p.first(), parseConstant(p.second()));
			}
			return vals;
		} else {
			// Problem
			syntaxError("constant expression expected", e);
			return null;
		}				
	}
	
	private Expr parseExpr() {
		checkNotEof();
		int start = index;
		Expr c1 = parseRelationalExpr();
		if (index < tokens.size() && tokens.get(index) instanceof LogicalAnd) {
			match("&&");
			Expr c2 = parseExpr();
			return new Expr.Binary(Expr.BOp.AND, c1, c2, sourceAttr(start, index - 1));
		} else if (index < tokens.size() && tokens.get(index) instanceof LogicalOr) {
			match("||");
			Expr c2 = parseExpr();
			return new Expr.Binary(Expr.BOp.OR, c1, c2, sourceAttr(start, index - 1));
		}
		return c1;
	}

	private Expr parseRelationalExpr() {
		int start = index;

		Expr lhs = parseAdditiveExpr();

		if (index < tokens.size() && tokens.get(index) instanceof LessEquals) {
			match("<=");
			Expr rhs = parseAdditiveExpr();
			return new Expr.Binary(Expr.BOp.LTEQ, lhs, rhs, sourceAttr(start, index - 1));
		} else if (index < tokens.size() && tokens.get(index) instanceof LeftAngle) {
			match("<");
			Expr rhs = parseAdditiveExpr();
			return new Expr.Binary(Expr.BOp.LT, lhs, rhs, sourceAttr(start, index - 1));
		} else if (index < tokens.size() && tokens.get(index) instanceof GreaterEquals) {
			match(">=");
			Expr rhs = parseAdditiveExpr();
			return new Expr.Binary(Expr.BOp.GTEQ, lhs, rhs, sourceAttr(start, index - 1));
		} else if (index < tokens.size() && tokens.get(index) instanceof RightAngle) {
			match(">");
			Expr rhs = parseAdditiveExpr();
			return new Expr.Binary(Expr.BOp.GT, lhs, rhs, sourceAttr(start, index - 1));
		} else if (index < tokens.size() && tokens.get(index) instanceof EqualsEquals) {
			match("==");
			Expr rhs = parseAdditiveExpr();
			return new Expr.Binary(Expr.BOp.EQ, lhs, rhs, sourceAttr(start, index - 1));
		} else if (index < tokens.size() && tokens.get(index) instanceof NotEquals) {
			match("!=");
			Expr rhs = parseAdditiveExpr();
			return new Expr.Binary(Expr.BOp.NEQ, lhs, rhs, sourceAttr(start, index - 1));
		} else {
			return lhs;
		}
	}

	private Expr parseAdditiveExpr() {
		int start = index;
		Expr lhs = parseMultplicativeExpr();

		if (index < tokens.size() && tokens.get(index) instanceof Plus) {
			match("+");
			Expr rhs = parseAdditiveExpr();
			return new Expr.Binary(Expr.BOp.ADD, lhs, rhs, sourceAttr(start, index - 1));
		} else if (index < tokens.size() && tokens.get(index) instanceof Minus) {
			match("-");
			Expr rhs = parseAdditiveExpr();
			return new Expr.Binary(Expr.BOp.SUB, lhs, rhs, sourceAttr(start, index - 1));
		}

		return lhs;
	}

	private Expr parseMultplicativeExpr() {
		int start = index;
		Expr lhs = parseIndexTerm();

		if (index < tokens.size() && tokens.get(index) instanceof Star) {
			match("*");
			Expr rhs = parseMultplicativeExpr();
			return new Expr.Binary(Expr.BOp.MUL, lhs, rhs, sourceAttr(start, index - 1));
		} else if (index < tokens.size() && tokens.get(index) instanceof RightSlash) {
			match("/");
			Expr rhs = parseMultplicativeExpr();
			return new Expr.Binary(Expr.BOp.DIV, lhs, rhs, sourceAttr(start, index - 1));
		} else if (index < tokens.size() && tokens.get(index) instanceof Percent) {
			match("%");
			Expr rhs = parseMultplicativeExpr();
			return new Expr.Binary(Expr.BOp.REM, lhs, rhs, sourceAttr(start, index - 1));
		}

		return lhs;
	}

	private Expr parseIndexTerm() {
		checkNotEof();
		int start = index;
		Expr lhs = parseTerm();

		Token lookahead = tokens.get(index);

		while (lookahead instanceof LeftSquare || lookahead instanceof Dot || lookahead instanceof LeftBrace) {
			if (lookahead instanceof LeftSquare) {
				match("[");
				Expr rhs = parseAdditiveExpr();
				match("]");
				lhs = new Expr.IndexOf(lhs, rhs, sourceAttr(start, index - 1));
			} else {
				match(".");
				String name = matchIdentifier().text;
				lhs = new Expr.RecordAccess(lhs, name, sourceAttr(start, index - 1));
			}
			if (index < tokens.size()) {
				lookahead = tokens.get(index);
			} else {
				lookahead = null;
			}
		}

		return lhs;
	}

	private Expr parseTerm() {
		checkNotEof();

		int start = index;
		Token token = tokens.get(index);

		if (token instanceof LeftBrace) {
			match("(");
			Expr e = parseExpr();
			checkNotEof();
			match(")");
			return e;
		} else if ((index + 1) < tokens.size() && token instanceof Identifier
				&& tokens.get(index + 1) instanceof LeftBrace) {
			// must be a method invocation
			return parseInvokeExprOrStmt();
		} else if (token.text.equals("null")) {
			matchKeyword("null");
			return new Expr.Constant(null, sourceAttr(start, index - 1));
		} else if (token.text.equals("true")) {
			matchKeyword("true");
			return new Expr.Constant(true, sourceAttr(start, index - 1));
		} else if (token.text.equals("false")) {
			matchKeyword("false");
			return new Expr.Constant(false, sourceAttr(start, index - 1));
		} else if (token instanceof Identifier) {
			return new Expr.Variable(matchIdentifier().text, sourceAttr(start, index - 1));
		} else if (token instanceof Lexer.Char) {
			char val = match(Lexer.Char.class, "a character").value;
			return new Expr.Constant(new Character(val), sourceAttr(start, index - 1));
		} else if (token instanceof Int) {
			int val = match(Int.class, "an integer").value;
			return new Expr.Constant(val, sourceAttr(start, index - 1));
		} else if (token instanceof Strung) {
			return parseString();
		} else if (token instanceof Minus) {
			return parseNegationExpr();
		} else if (token instanceof Bar) {
			return parseArrayLengthExpr();
		} else if (token instanceof LeftSquare) {
			return parseArrayInitialiserOrGeneratorExpr();
		} else if (token instanceof LeftCurly) {
			return parseRecordInitialiserExpr();
		} else if (token instanceof Shreak) {
			match("!");
			return new Expr.Unary(Expr.UOp.NOT, parseTerm(), sourceAttr(start, index - 1));
		}
		syntaxError("unrecognised term (\"" + token.text + "\")", token);
		return null;
	}

	private Expr parseArrayInitialiserOrGeneratorExpr() {
		int start = index;
		ArrayList<Expr> exprs = new ArrayList<Expr>();
		match("[");
		checkNotEof();
		Token token = tokens.get(index);
		// Check for array generator expression
		if (!(token instanceof RightSquare)) {
			exprs.add(parseExpr());
			checkNotEof();
			token = tokens.get(index);
			if (token instanceof SemiColon) {
				// Array generator
				match(";");
				exprs.add(parseExpr());
				checkNotEof();
				match("]");
				return new Expr.ArrayGenerator(exprs.get(0), exprs.get(1), sourceAttr(start, index - 1));
			} else {
				// Array initialiser
				while (!(token instanceof RightSquare)) {
					match(",");
					exprs.add(parseExpr());
					checkNotEof();
					token = tokens.get(index);
				}
			}
		}

		match("]");
		return new Expr.ArrayInitialiser(exprs, sourceAttr(start, index - 1));
	}

	private Expr parseRecordInitialiserExpr() {
		int start = index;
		match("{");
		HashSet<String> keys = new HashSet<String>();
		ArrayList<Pair<String, Expr>> exprs = new ArrayList<Pair<String, Expr>>();
		checkNotEof();
		Token token = tokens.get(index);
		boolean firstTime = true;
		while (!(token instanceof RightCurly)) {
			if (!firstTime) {
				match(",");
			}
			firstTime = false;

			checkNotEof();
			token = tokens.get(index);
			Identifier n = matchIdentifier();

			if (keys.contains(n.text)) {
				syntaxError("duplicate tuple key", n);
			}

			match(":");

			Expr e = parseExpr();
			exprs.add(new Pair<String, Expr>(n.text, e));
			keys.add(n.text);
			checkNotEof();
			token = tokens.get(index);
		}
		match("}");
		return new Expr.RecordConstructor(exprs, sourceAttr(start, index - 1));
	}

	private Expr parseArrayLengthExpr() {
		int start = index;
		match("|");
		Expr e = parseIndexTerm();
		match("|");
		return new Expr.Unary(Expr.UOp.LENGTHOF, e, sourceAttr(start, index - 1));
	}

	private Expr parseNegationExpr() {
		int start = index;
		match("-");
		Expr e = parseIndexTerm();

		if (e instanceof Expr.Constant) {
			Expr.Constant c = (Expr.Constant) e;
			if (c.getValue() instanceof Integer) {
				int bi = (Integer) c.getValue();
				return new Expr.Constant(-bi, sourceAttr(start, index));
			}
		}

		return new Expr.Unary(Expr.UOp.NEG, e, sourceAttr(start, index));
	}

	private Expr.Invoke parseInvokeExprOrStmt() {
		int start = index;
		Identifier name = matchIdentifier();
		match("(");
		boolean firstTime = true;
		ArrayList<Expr> args = new ArrayList<Expr>();
		while (index < tokens.size() && !(tokens.get(index) instanceof RightBrace)) {
			if (!firstTime) {
				match(",");
			} else {
				firstTime = false;
			}
			Expr e = parseExpr();

			args.add(e);
		}
		match(")");
		return new Expr.Invoke(name.text, args, sourceAttr(start, index - 1));
	}

	private Expr parseString() {
		int start = index;
		String s = match(Strung.class, "a string").string;
		return new Expr.Constant(s, sourceAttr(start, index - 1));
	}

	private Type parseType() {
		int start = index;
		checkNotEof();
		// Determine base type
		Type type = parseBaseType();
		// Determine array level (if any)
		while (index < tokens.size() && tokens.get(index) instanceof LeftSquare) {
			match("[");
			match("]");
			type = new Type.Array(type, sourceAttr(start, index - 1));
		}
		// Done
		return type;
	}

	/**
	 * Parse a "base" type. That is any type which could be the element of an
	 * array type.
	 * 
	 * @return
	 */
	private Type parseBaseType() {
		int start = index;
		Token token = tokens.get(index);
		if (token.text.equals("int")) {
			matchKeyword("int");
			return new Type.Int(sourceAttr(start, index - 1));
		} else if (token.text.equals("void")) {
			matchKeyword("void");
			return new Type.Void(sourceAttr(start, index - 1));
		} else if (token.text.equals("bool")) {
			matchKeyword("bool");
			return new Type.Bool(sourceAttr(start, index - 1));
		} else if (token.text.equals("char")) {
			matchKeyword("char");
			return new Type.Char(sourceAttr(start, index - 1));
		} else if (token.text.equals("string")) {
			matchKeyword("string");
			return new Type.Strung(sourceAttr(start, index - 1));
		} else if (token instanceof LeftCurly) {
			// record type
			return parseRecordType();
		} else {
			Identifier id = matchIdentifier();
			return new Type.Named(id.text, sourceAttr(start, index - 1));
		}
	}

	/**
	 * Parse a record type, which takes the form:
	 * 
	 * <pre>
	 * RecordType ::= '{' Type Indent ( ',' Type Indent )* '}'
	 * </pre>
	 * 
	 * This function additionally checks that no two fields have the same name.
	 * 
	 * @return
	 */
	private Type.Record parseRecordType() {
		int start = index;
		match("{");
		// The fields set tracks the field names we've already seen
		HashSet<String> fields = new HashSet<String>();
		ArrayList<Pair<Type,String>> types = new ArrayList<Pair<Type,String>>();
		
		Token token = tokens.get(index);
		boolean firstTime = true;
		while (!(token instanceof RightCurly)) {
			if (!firstTime) {
				match(",");
			}
			firstTime = false;
			checkNotEof();
			
			token = tokens.get(index);
			Type type = parseType();
			Identifier n = matchIdentifier();

			if (fields.contains(n.text)) {
				syntaxError("duplicate field", n);
			}
			types.add(new Pair<Type,String>(type,n.text));
			fields.add(n.text);
			checkNotEof();
			token = tokens.get(index);
		}
		match("}");
		return new Type.Record(types, sourceAttr(start, index - 1));
	}

	private void checkNotEof() {
		if (index >= tokens.size()) {
			throw new SyntaxError("unexpected end-of-file", filename, index - 1, index - 1);
		}
		return;
	}

	private Token match(String op) {
		checkNotEof();
		Token t = tokens.get(index);
		if (!t.text.equals(op)) {
			syntaxError("expecting '" + op + "', found '" + t.text + "'", t);
		}
		index = index + 1;
		return t;
	}

	@SuppressWarnings("unchecked")
	private <T extends Token> T match(Class<T> c, String name) {
		checkNotEof();
		Token t = tokens.get(index);
		if (!c.isInstance(t)) {
			syntaxError("expecting " + name + ", found '" + t.text + "'", t);
		}
		index = index + 1;
		return (T) t;
	}

	private Identifier matchIdentifier() {
		checkNotEof();
		Token t = tokens.get(index);
		if (t instanceof Identifier) {
			Identifier i = (Identifier) t;
			index = index + 1;
			return i;
		}
		syntaxError("identifier expected", t);
		return null; // unreachable.
	}

	private Keyword matchKeyword(String keyword) {
		checkNotEof();
		Token t = tokens.get(index);
		if (t instanceof Keyword) {
			if (t.text.equals(keyword)) {
				index = index + 1;
				return (Keyword) t;
			}
		}
		syntaxError("keyword " + keyword + " expected.", t);
		return null;
	}

	private Attribute.Source sourceAttr(int start, int end) {
		Token t1 = tokens.get(start);
		Token t2 = tokens.get(end);
		return new Attribute.Source(t1.start, t2.end());
	}

	private void syntaxError(String msg, Expr e) {
		Attribute.Source loc = e.attribute(Attribute.Source.class);
		throw new SyntaxError(msg, filename, loc.start, loc.end);
	}

	private void syntaxError(String msg, Token t) {
		throw new SyntaxError(msg, filename, t.start, t.start + t.text.length() - 1);
	}
}
