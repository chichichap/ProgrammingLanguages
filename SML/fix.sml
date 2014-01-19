(* description: this function takes a tuple of a function and an element, and applied f to x until a fixed point is reached
 *              this is a helper function of fixA. 
 *              this function uses recursion.
 * type: fn : (''a -> ''a) * ''a -> ''a
 * argument: (f, x) : (''a -> ''a) * ''a
 * preconditions: 1. x is a fixed point of function f if f (x) = x .
 * 				  2. findFix may not terminate for all inputs
 *                3. f is a unary function
 *                4. x is of type ''a and can be applied by f
 * postconditions: 1. the returned value is the fixed point of function f 
 *                 2. the returned value is of type ''a. *)	
fun findFix (f, x) = let val y = f(x)
				 in if y = x then x else findFix(f, y)
				 end;

(* description: this function takes a unary function f as input, and returns a function that applies f to its argument repeatedly until a fixed point is reached.
 *              this funtion calls a recursive helper function. findFixA.
 *              this function does not use recursion.
 * type: fn : (''a -> ''a) -> ''a -> ''a
 * argument: f : (''a -> ''a)
 * preconditions: 1. fixA may not terminate for all inputs
 *                2. f is a unary function
 * postconditions: 1. the returned value is a function that applies f to its argument repeatedly until a fixed point is reached.
 *                 2. the returned value is of type ''a -> ''a. *)	
fun fixA f = (fn x => findFix(f, x));

(* description: this function takes a unary function f as input, and returns a function that applies f to its argument repeatedly until a fixed point is reached.
 *              this funtion uses recursion and no helper function.
 *              each recursive call returns an unnamed function expression.
 * type: fn : (''a -> ''a) -> ''a -> ''a
 * argument: f : (''a -> ''a)
 * preconditions: 1. fixB may not terminate for all inputs
 *                2. f is a unary function
 * postconditions: 1. the returned value is a function that applies f to its argument repeatedly until a fixed point is reached.
 *                 2. the returned value is of type ''a -> ''a. *)	
fun fixB f = (fn x => let val y = f(x)
			          in if y = x then x else (fixB f) y
					  end);

(* description: this function takes a unary function f as input, and returns a function that applies f to its argument repeatedly until a fixed point is reached.
 *              this funtion uses no helper function and no unnamed functions.
 *              this function uses currying.
 * type: fn : (''a -> ''a) -> ''a -> ''a
 * argument: f : (''a -> ''a)
 * preconditions: 1. fixC may not terminate for all inputs
 *                2. f is a unary function
 * postconditions: 1. the returned value is a function that applies f to its argument repeatedly until a fixed point is reached.
 *                 2. the returned value is of type ''a -> ''a. *)						  
fun fixC f x = let val y = f(x)
				in if y = x then x else (fixC f) y
				end;
