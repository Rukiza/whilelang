package whilelang.compiler;

import whilelang.ast.*;
import whilelang.util.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import static whilelang.util.SyntaxError.internalFailure;

/**
 * Created by Shane on 12/03/16.
 */
public class MacroExpansion {
    private WhileFile file;
    private WhileFile.MethodDecl method;
    private HashMap<String,WhileFile.MethodDecl> methods;
    private HashMap<String, WhileFile.MacroDecl> macros;

    public WhileFile check(WhileFile wf) {

        this.file = wf;
        this.methods = new HashMap<String,WhileFile.MethodDecl>();
        this.macros = new HashMap<String, WhileFile.MacroDecl>();
        List<WhileFile.Decl> decls = new ArrayList<WhileFile.Decl>();

        for(WhileFile.Decl declaration : wf.declarations) {
            if(declaration instanceof WhileFile.MethodDecl) {
                WhileFile.MethodDecl fd = (WhileFile.MethodDecl) declaration;
                this.methods.put(fd.name(), fd);
            } else if(declaration instanceof WhileFile.MacroDecl) {
                WhileFile.MacroDecl fd = (WhileFile.MacroDecl) declaration;
                if (macros.containsKey(fd.getName())) {
                    //Todo: Error here for duplicate macro names.
                }
                this.macros.put(fd.name(), fd);
            } else if(declaration instanceof WhileFile.TypeDecl) {
                decls.add(declaration);
            }
        }
        for(WhileFile.Decl declaration : wf.declarations) {
            if(declaration instanceof WhileFile.MethodDecl) {
                decls.add(check((WhileFile.MethodDecl) declaration));
            }
        }

        return new WhileFile(file.filename, decls);
    }

    private void check() {
        //Todo: Method and Macro declaration matching check. - if needed.
    }

    private WhileFile.MethodDecl check(WhileFile.MethodDecl declaration) {
        List<String> enviroment = new ArrayList<String>();

        for (WhileFile.Parameter parameter : declaration.getParameters()){
            enviroment.add(parameter.name());
        }

        return new WhileFile.MethodDecl(declaration.getName(),
                declaration.getRet(),
                declaration.getParameters() ,
                check(declaration.getBody(), enviroment),
                declaration.attributes().toArray(new Attribute[declaration.attributes().size()])) ;
    }

    private List<Stmt> check(List<Stmt> body, List<String> environment) {
        List<Stmt> newBody = new ArrayList<Stmt>();
        for (Stmt stmt : body){
            newBody.add(check(stmt, environment));
        }
        return newBody;
    }

    private Stmt check(Stmt stmt, List<String> environment) {
        if (stmt instanceof Stmt.Assert) {
            return check((Stmt.Assert) stmt,environment);
        } else if (stmt instanceof Stmt.Assign) {
            return check((Stmt.Assign) stmt, environment);
        } else if (stmt instanceof Stmt.Return) {
            return check((Stmt.Return) stmt, environment);
        } else if (stmt instanceof Stmt.Print) {
            return check((Stmt.Print) stmt, environment);
        } else if(stmt instanceof Stmt.Break) {
            return stmt;// nothing to do
        } else if(stmt instanceof Stmt.Continue) {
            return stmt; // nothing to do
        } else if(stmt instanceof Stmt.VariableDeclaration) {
            return check((Stmt.VariableDeclaration) stmt, environment);
        } else if(stmt instanceof Expr.Invoke) {
            return (Stmt) check((Expr.Invoke) stmt, environment);
        } else if(stmt instanceof Stmt.IfElse) {
            return check((Stmt.IfElse) stmt, environment);
        } else if(stmt instanceof Stmt.For) {
            return check((Stmt.For) stmt, environment);
        } else if(stmt instanceof Stmt.While) {
            return check((Stmt.While) stmt, environment);
        } else if (stmt instanceof  Stmt.DoWhile) {
            return check((Stmt.DoWhile) stmt, environment);
        } else if(stmt instanceof Stmt.Switch) {
            return check((Stmt.Switch) stmt, environment);
        } else {
            internalFailure("unknown statement encountered (" + stmt + ")", file.filename,stmt);
            return null;//Dead code
        }

    }

    private Stmt.Assert check(Stmt.Assert stmt, List<String> environment) {
        return new Stmt.Assert(check(stmt.getExpr(), environment), stmt.attributes());
    }

    private Stmt.Assign check(Stmt.Assign stmt, List<String> environment) {
        return new Stmt.Assign(stmt.getLhs(),check(stmt.getRhs(), environment), stmt.attributes());
    }

    private Stmt.Return check(Stmt.Return stmt, List<String> environment) {
        if (stmt.getExpr() == null) {
            return new Stmt.Return(null, stmt.attributes());
        }
        return new Stmt.Return(check(stmt.getExpr(), environment), stmt.attributes());
    }

    private Stmt.Print check(Stmt.Print stmt, List<String> environment) {
        return new Stmt.Print(check(stmt.getExpr(), environment), stmt.attributes());
    }

    private Stmt.VariableDeclaration check(Stmt.VariableDeclaration stmt, List<String> environment) {
        Stmt.VariableDeclaration newStmt = null;
        if (stmt.getExpr() == null){
            newStmt = new Stmt.VariableDeclaration(stmt.getType(),
                    stmt.getName(),
                    null);
        } else {
             newStmt = new Stmt.VariableDeclaration(stmt.getType(),
                    stmt.getName(),
                    check(stmt.getExpr(), environment));
            environment.add(stmt.getName());
        }
        return newStmt;
    }

    private Stmt.IfElse check(Stmt.IfElse stmt, List<String> environment) {
        return new Stmt.IfElse(check(stmt.getCondition(), environment),
                check(stmt.getTrueBranch(), environment),
                check(stmt.getFalseBranch(), environment),
                stmt.attributes());
    }

    private Stmt.For check(Stmt.For stmt, List<String> environment) {
        return new Stmt.For(check(stmt.getDeclaration(), environment),
                check(stmt.getCondition(), environment),
                check(stmt.getIncrement(), environment),
                check(stmt.getBody(), environment));

    }

    private Stmt.While check(Stmt.While stmt, List<String> environment) {
        return new Stmt.While(check(stmt.getCondition(), environment),
                null, //Invariant that is not used
                check(stmt.getBody(), environment),
                stmt.attributes());
    }

    private Stmt.DoWhile check(Stmt.DoWhile stmt, List<String> environment) {
        return new Stmt.DoWhile(
                check(stmt.getCondition(), environment),
                null, //Inverent that is not used
                check(stmt.getBody(), environment),
                stmt.attributes());
    }

    private Stmt.Switch check(Stmt.Switch stmt, List<String> environment) {
        List<Stmt.Case> cases = new ArrayList<Stmt.Case>();
        for (Stmt.Case c: stmt.getCases()){
            cases.add(check(c, environment));
        }
        return new Stmt.Switch(check(stmt.getExpr(), environment),
                cases,
                stmt.attributes());
    }

    private Stmt.Case check(Stmt.Case stmt, List<String> enviroment) {
        // Might be a problem with the implementation of switch
        if (stmt.getValue() == null){
            return new Stmt.Case(null, check(stmt.getBody(), enviroment), stmt.attributes());
        }
        return new Stmt.Case((Expr.Constant) check(stmt.getValue(), enviroment), check(stmt.getBody(), enviroment), stmt.attributes());
    }

    private Expr check(Expr expr, List<String> environment) {
        if(expr instanceof Expr.Binary) {
            return check((Expr.Binary) expr, environment);
        } else if(expr instanceof Expr.Constant) {
            return expr;//Do nothing
        } else if(expr instanceof Expr.IndexOf) {
            return check((Expr.IndexOf) expr, environment);
        } else if(expr instanceof Expr.Invoke) {
            return check((Expr.Invoke) expr, environment);
        } else if(expr instanceof Expr.ArrayGenerator) {
            return check((Expr.ArrayGenerator) expr, environment);
        } else if(expr instanceof Expr.ArrayInitialiser) {
            return check((Expr.ArrayInitialiser) expr, environment);
        } else if(expr instanceof Expr.RecordAccess) {
            return check((Expr.RecordAccess) expr, environment);
        } else if(expr instanceof Expr.RecordConstructor) {
            return check((Expr.RecordConstructor) expr, environment);
        } else if(expr instanceof Expr.Unary) {
            return check((Expr.Unary) expr, environment);
        } else if(expr instanceof Expr.Variable) {
            return expr;//Do nothing
        } else {
            internalFailure("unknown expression encountered (" + expr + ")", file.filename,expr);
            return null;
        }
    }

    private Expr check(Expr.Invoke expr, List<String> environment) {
        if (methods.containsKey(expr.getName()) && macros.containsKey(expr.getName())) {
            internalFailure("method and macro cross over name spaces (" + expr + ")", file.filename, expr);
        }
        else if (methods.containsKey(expr.getName())) {
            // Check the method for macros
            List<Expr> args = new ArrayList<Expr>();
            for (Expr e: expr.getArguments()) {
                args.add(check(e, environment));
            }
            return new Expr.Invoke(expr.getName(), args, expr.attributes().toArray(new Attribute[expr.attributes().size()]));
        }
        else if (macros.containsKey(expr.getName())) {
            WhileFile.MacroDecl macro = macros.get(expr.getName());

            //Todo: variable replacement and macro replacement.
            ExprFactory factory = new ExprFactory(macro.getExpr());
            Expr newExpr = factory.makeExpr(macro.getMacroParameters(), expr.getArguments());

            //Todo: Deside on if this is done before the replacement of after. Possibly after.
            return check(newExpr, environment);

        }
        else {
            internalFailure("there is not method or macro that matches (" + expr + ")", file.filename, expr);
            return null; //Also Dead
        }
        return null;// Dead code.
    }

    private Expr.Binary check(Expr.Binary expr, List<String> environment) {
        return new Expr.Binary(expr.getOp(),
                check(expr.getLhs(), environment),
                check(expr.getRhs(),environment),
                expr.attributes());
    }

    private Expr.IndexOf check(Expr.IndexOf expr, List<String> environment) {
        return new Expr.IndexOf(check(expr.getSource(), environment),
                check(expr.getIndex(),environment),
                expr.attributes());
    }

    private Expr.ArrayGenerator check(Expr.ArrayGenerator expr, List<String> environment) {
        return new Expr.ArrayGenerator(check(expr.getValue(), environment),
                check(expr.getSize(),environment),
                expr.attributes().toArray(new Attribute[expr.attributes().size()]));
    }

    private Expr.ArrayInitialiser check(Expr.ArrayInitialiser expr, List<String> environment) {
        List<Expr> args = new ArrayList<Expr>();
        for (Expr e: expr.getArguments()){
            args.add(check(e, environment));
        }
        return new Expr.ArrayInitialiser(args, expr.attributes().toArray(new Attribute[expr.attributes().size()]));
    }

    private Expr.RecordAccess check(Expr.RecordAccess expr, List<String> environment) {
        return new Expr.RecordAccess(check(expr.getSource(),
                environment), expr.getName(),
                expr.attributes().toArray(new Attribute[expr.attributes().size()]));
    }

    private Expr.RecordConstructor check(Expr.RecordConstructor expr, List<String> environment) {
        List<Pair<String, Expr>> arguments = expr.getFields();
        List<Pair<String, Expr>> newArgs = new ArrayList<Pair<String, Expr>>();
        for (Pair<String, Expr> p : arguments) {
            newArgs.add(new Pair<String, Expr>(p.first(),check(p.second(), environment)));
        }
        return new Expr.RecordConstructor(newArgs, expr.attributes().toArray(new Attribute[expr.attributes().size()]));
    }

    private Expr.Unary check(Expr.Unary expr, List<String> environment) {
        return new Expr.Unary(expr.getOp(),
                check(expr.getExpr(), environment),
                expr.attributes().toArray(new Attribute[expr.attributes().size()]));
    }

    /**
     * Pattern building factory
     */
    private class ExprFactory {

        private final Expr pattern;

        public ExprFactory (Expr expr) {
            this.pattern = expr;
        }

        public Expr makeExpr(){
            return makeExpr(pattern, new ArrayList<WhileFile.MacroParameter>(), new ArrayList<Expr>());
        }

        public Expr makeExpr(List<WhileFile.MacroParameter> macroParameters, List<Expr> arguments){
            return makeExpr(pattern, macroParameters, arguments);
        }

        private Expr makeExpr(Expr pattern, List<WhileFile.MacroParameter> macroParameters, List<Expr> arguments){
            Expr expr = null;

            if(pattern instanceof Expr.Binary) {
                expr = makeExpr((Expr.Binary) pattern, macroParameters, arguments);
            } else if(pattern instanceof Expr.Constant) {
                expr = makeExpr((Expr.Constant) pattern);
            } else if(pattern instanceof Expr.IndexOf) {
                expr = makeExpr((Expr.IndexOf) pattern, macroParameters, arguments);
            } else if(pattern instanceof Expr.Invoke) {
                expr = makeExpr((Expr.Invoke) pattern, macroParameters, arguments);
            } else if(pattern instanceof Expr.ArrayGenerator) {
                expr = makeExpr((Expr.ArrayGenerator) pattern, macroParameters, arguments);
            } else if(pattern instanceof Expr.ArrayInitialiser) {
                expr = makeExpr((Expr.ArrayInitialiser) pattern, macroParameters, arguments);
            } else if(pattern instanceof Expr.RecordAccess) {
                expr = makeExpr((Expr.RecordAccess) pattern, macroParameters, arguments);
            } else if(pattern instanceof Expr.RecordConstructor) {
                expr = makeExpr((Expr.RecordConstructor) pattern, macroParameters, arguments);
            } else if(pattern instanceof Expr.Unary) {
                expr = makeExpr((Expr.Unary) pattern, macroParameters, arguments);
            } else if(pattern instanceof Expr.Variable) {
                expr = makeExpr((Expr.Variable) pattern, macroParameters, arguments);
            }
            return expr;
        }

        private Expr makeExpr(Expr.Variable pattern, List<WhileFile.MacroParameter> macroParameters, List<Expr> arguments) {
            Expr expr = new Expr.Variable(pattern.getName(),
                    pattern.attributes().toArray(new Attribute[pattern.attributes().size()]));
            // Do some more work here.
            String patternName = pattern.getName();
            for (int i = 0; i < macroParameters.size(); i++){
                if (macroParameters.get(i).getName().equals(patternName)) {
                    ExprFactory factory = new ExprFactory(arguments.get(i));
                    return factory.makeExpr();
                }
            }
            return expr;
        }

        private Expr makeExpr(Expr.Unary pattern, List<WhileFile.MacroParameter> macroParameters, List<Expr> arguments) {
            Expr expr = new Expr.Unary(pattern.getOp(),
                    makeExpr(pattern.getExpr(), macroParameters, arguments),
                    pattern.attributes().toArray(new Attribute[pattern.attributes().size()]));
            return expr;
        }

        private Expr makeExpr(Expr.RecordConstructor pattern, List<WhileFile.MacroParameter> macroParameters, List<Expr> arguments) {
            List<Pair<String, Expr>> contents = new ArrayList<Pair<String, Expr>>();
            for (Pair<String, Expr> pair: pattern.getFields()){
                Pair<String, Expr> newPair = new Pair<String, Expr>(pair.first(), makeExpr(pair.second(), macroParameters, arguments));
                contents.add(newPair);
            }
            return new Expr.RecordConstructor(contents, pattern.attributes().toArray(new Attribute[pattern.attributes().size()]));
        }

        private Expr makeExpr(Expr.RecordAccess pattern ,List<WhileFile.MacroParameter> macroParameters, List<Expr> arguments) {
            return new Expr.RecordAccess(makeExpr(pattern.getSource(), macroParameters, arguments),
                    pattern.getName(),
                    pattern.attributes().toArray(new Attribute[pattern.attributes().size()]));
        }

        private Expr makeExpr(Expr.ArrayInitialiser pattern, List<WhileFile.MacroParameter> macroParameters, List<Expr> arguments) {
            List<Expr> args = new ArrayList<Expr>();
            for (Expr argument: pattern.getArguments()){
                args.add(makeExpr(argument, macroParameters, arguments));
            }
            return new Expr.ArrayInitialiser(args, pattern.attributes().toArray(new Attribute[pattern.attributes().size()]));
        }

        private Expr makeExpr(Expr.ArrayGenerator pattern, List<WhileFile.MacroParameter> macroParameters, List<Expr> arguments) {
            return new Expr.ArrayGenerator(makeExpr(pattern.getValue(), macroParameters, arguments),
                    makeExpr(pattern.getSize(), macroParameters, arguments),
                    pattern.attributes().toArray(new Attribute[pattern.attributes().size()]));
        }

        private Expr makeExpr(Expr.Invoke pattern, List<WhileFile.MacroParameter> macroParameters, List<Expr> arguments) {
            List<Expr> args = new ArrayList<Expr>();
            for (Expr arg: pattern.getArguments()){
                args.add(makeExpr(arg, macroParameters, arguments));
            }
            return new Expr.Invoke(pattern.getName(),
                    args,
                    pattern.attributes().toArray(new Attribute[pattern.attributes().size()]));
        }

        private Expr makeExpr(Expr.IndexOf pattern, List<WhileFile.MacroParameter> macroParameters, List<Expr> arguments) {
            return new Expr.IndexOf(makeExpr(pattern.getSource(), macroParameters, arguments),
                    makeExpr(pattern.getIndex(), macroParameters, arguments),
                    pattern.attributes().toArray(new Attribute[pattern.attributes().size()]));
        }

        private Expr makeExpr(Expr.Constant pattern) {
            return new Expr.Constant(pattern.getValue(), pattern.attributes().toArray(new Attribute[pattern.attributes().size()]));
        }

        private Expr makeExpr(Expr.Binary pattern, List<WhileFile.MacroParameter> macroParameters, List<Expr> arguments) {
            return new Expr.Binary(pattern.getOp(),
                    makeExpr(pattern.getLhs(), macroParameters, arguments),
                    makeExpr(pattern.getRhs(), macroParameters, arguments),
                    pattern.attributes().toArray(new Attribute[pattern.attributes().size()]));
        }

    }

}
