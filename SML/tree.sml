datatype ''a tree = Node of ''a * ''a tree list;

(* description: this function takes an element x and a tree t, and returns the number of times that x appears as a node label in t. 
 *              this funtion uses recursion and does not use HOPs.        
 * type: fn : ''a * ''a tree -> int
 * argument: (x, t) : ''a * ''a tree
 * preconditions: 1. each node can have any number of children.
 * 				  2. each node in tree has a label that can be of any type but real
 *                3. each node in tree is followed by a list of its children, each representing a tree itself
 *                4. t is of type ''a tree
 *                5. x and root are of type ''a (comparable).
 * postconditions: 1. the returned value is the number of times that x appears as a node label in t. 
 *                 2. the returned value is of type int. *)
fun count (x, Node (root, [])) = if x = root then 1 else 0
  | count (x, Node (root, (Node (root2, list2))::tl)) = count (x, Node (root2, list2)) 
													  + count (x, Node (root, tl));
													  
(* description: this function takes an element x and a tree t, and returns the number of times that x appears as a node label in t. 
 *              this funtion uses recursion and two HOPs: foldr and map    
 * type: fn : ''a * ''a tree -> int
 * argument: (x, t) : ''a * ''a tree
 * preconditions: 1. each node can have any number of children.
 * 				  2. each node in tree has a label that can be of any type but real
 *                3. each node in tree is followed by a list of its children, each representing a tree itself
 *                4. t is of type ''a tree
 *                5. x and root are of type ''a (comparable).
 * postconditions: 1. the returned value is the number of times that x appears as a node label in t. 
 *                 2. the returned value is of type int. *)											  											  
fun countHOP (x, Node (root, lst)) = let 	val countRoot = if x = root then 1 else 0
											fun countChild (Node (root2, lst2)) = countHOP (x, Node (root2, lst2))
											fun add (A, B) = A + B
								     in		countRoot + (foldr add 0 (map countChild lst))
									 end;

(* description: this function takes a tree and returns its depth, the maximum length of a path from the root of the tree to a leaf.
 *              this funtion uses recursion and does not use HOPs.
 * type: fn : ''a * ''a tree -> int
 * argument: (x, t) : ''a * ''a tree
 * preconditions: 1. each node can have any number of children.
 * 				  2. each node in tree has a label that can be of any type but real
 *                3. each node in tree is followed by a list of its children, each representing a tree itself
 *                4. a single node has a depth of zero
 *                5. t is of type ''a tree
 *                6. root is of type ''a.
 * postconditions: 1. the returned value is the maximum length of a path from the root of the tree to a leaf. 
 *                 2. the returned value is of type int. *)								 
fun depth (Node (root, [])) = 0
  | depth (Node (root, (Node (root2, list2))::tl)) = let 	val A = depth (Node (root2, list2)) + 1
															val B = depth (Node (root , tl))
													 in	 	if (A > B) then A else B
													 end;

(* description: this function takes a tree and returns its depth, the maximum length of a path from the root of the tree to a leaf.
 *              this funtion uses recursion and two HOPs: foldr and map    
 * type: fn : ''a * ''a tree -> int
 * argument: (x, t) : ''a * ''a tree
 * preconditions: 1. each node can have any number of children.
 * 				  2. each node in tree has a label that can be of any type but real
 *                3. each node in tree is followed by a list of its children, each representing a tree itself
 *                4. a single node has a depth of zero
 *                5. t is of type ''a tree
 *                6. root is of type ''a.
 * postconditions: 1. the returned value is the maximum length of a path from the root of the tree to a leaf. 
 *                 2. the returned value is of type int. *)													 
fun depthHOP (Node (root, lst)) = let	fun depthChild (Node (root2, lst2)) = depthHOP (Node (root2, lst2)) + 1
										fun max (A, B) = if (A > B) then A else B
								  in    foldr max 0 (map depthChild lst)
								  end;