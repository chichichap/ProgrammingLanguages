type Name = string;

datatype Expr = Const of int
	| Var of Name
	| Neg of Expr
	| Plus of Expr * Expr
	| Mult of Expr * Expr
	| App of Fun * Expr
and Fun = Def of Name * Expr;

(* description: this function takes a tuple of a variable and two expressions 
 *              and returns a new expression where every occurrence of the variable in the the the seconnd expression is replaced with the first expression           
 * type: fn : Name * Expr * Expr -> Expr
 * argument: (n, e1, e2) : Name * Expr * Expr
 * preconditions: 1. n is not bound by any function-definition within e2.
 * 				  2. No variable appearing in e1 is bound by a function-definition in e2.
 * 				  3. Definitions in e2 have fresh variable names, that is: no name is bound by more than one definition in e2, 
 *					 and that no name bound by a definition appears outside the body of the definition.
 *                4. n is of type Name
 *                5. e1 and e2 are of type Expr
 * postconditions: 1. the returned expression is the result of replacing every occurrence of n in e2 by e1 
 *                 2. the returned value is of type Expr. *)
fun substitute (n, e1, Const e2) = Const e2
  | substitute (n, e1, Var e2) = if n = e2 then e1 else Var e2
  | substitute (n, e1, Neg e2) = Neg (substitute(n, e1, e2))
  | substitute (n, e1, Plus (e2a, e2b)) = Plus (substitute(n, e1, e2a), substitute(n, e1, e2b))
  | substitute (n, e1, Mult (e2a, e2b)) = Mult (substitute(n, e1, e2a), substitute(n, e1, e2b))
  | substitute (n, Var e1, App (Def (e2a, e2b), e2c)) = if n = e2a then App (Def (e1, substitute(n, Var e1, e2b)), substitute(n, Var e1, e2c))
																   else App (Def (e2a, substitute(n, Var e1, e2b)), substitute(n, Var e1, e2c))
  | substitute (n, e1, App (Def (e2a, e2b), e2c)) = App (Def (e2a, substitute(n, e1, e2b)), substitute(n, e1, e2c));
  
 exception EvalException;
 
(* description: this function reduces a given closed expression to an integer by evaluating the expression.         
 * type: fn : Expr -> int
 * argument: e : Expr
 * preconditions: 1. A closed expression is one that does not contain variables that are not mentioned in the function definitions within the expression.
 *                2. e is of type Expr
 *                3. e1 and e2 are of type Expr
 *                4. p is of type Fun
 * postconditions: 1. Function applications, App(f,e), are evaluated by evaluating the body of the applied function (f: Fun)
 * 					  after substituting every instance of the function’s bound variable with the value of the second part of the function application
 *				   2. if an expression is not closed, then the evaluator function will raise an exception, EvalException. 
 *                 3. the returned value is of type int. *)
 fun eval (Const e) = e
   | eval (Var e) = raise EvalException
   | eval (Neg e) = ~(eval (e))
   | eval (Plus (e1, e2)) = eval (e1) + eval (e2)
   | eval (Mult (e1, e2)) = eval (e1) * eval (e2)
   | eval (App (Def (p, def), e)) = eval (substitute (p, e, def));
