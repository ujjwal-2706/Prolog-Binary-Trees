%Below is the defination of a binary tree in prolog
ibt(empty).
ibt(node(N,LBT,RBT)):- integer(N),ibt(LBT),ibt(RBT).

% This is a helper predicate to get max of 2 elements A,B
max(A,B,X):- A > B,X is A.
max(A,B,X):- B >= A ,X is B.


% Below we have a test case where 3 is root node with 2 nodes more at different levels
tree1(node(1,node(2,node(3,empty,empty),node(4,empty,empty)),node(5,empty,empty))).
tree1(node(3,node(1,empty,node(4,empty,empty)),empty)).
tree1(node(5,node(4,node(3,empty,empty),node(5,empty,empty)),node(7,node(6,empty,empty),empty))).


%For the size and height rule we just simply recursively check for the right and left binary tree
size(empty,0).
size(node(_N1,LBT,RBT),N):- size(LBT,X),size(RBT,Y),N is 1 + X + Y.
height(empty,0).
height(node(_N1,LBT,RBT),N) :- height(LBT,X),height(RBT,Y),max(X,Y,A),N is 1 + A.


% preorder, postorder, inorder can be obtained recursively simply by their defination
preorder(empty,[]).
preorder(node(N,LBT,RBT),L):- preorder(LBT,X),preorder(RBT,Y), append(X,Y,Z),append([N],Z,L).
inorder(empty,[]).
inorder(node(N,LBT,RBT),L):- inorder(LBT,X), inorder(RBT,Y),append(X,[N],Z),append(Z,Y,L).
postorder(empty,[]).
postorder(node(N,LBT,RBT),L):- postorder(LBT,X),postorder(RBT,Y),append(X,Y,Z),append(Z,[N],L).


% in order to do the tail recursive version of preorder,postorder and inorder we will,
% store the root in stack then pop it off and then 
% maintain a result list and pop the stack value and append it in result list
% check for right node of poped then put it in stack else put left and continue
% till stack is empty, though in case of postorder we will need 2 stacks
preorderHelper([],[]). % when stack is empty then true as we have to stop
preorderHelper([node(N,empty,empty)|T],[N|Result]):- preorderHelper(T,Result).
preorderHelper([node(N,empty,RBT) |T],[N|Result]):-append([RBT],T,Z),preorderHelper(Z,Result).
preorderHelper([node(N,LBT,empty)|T],[N|Result]):- append([LBT],T,Z), preorderHelper(Z,Result).
preorderHelper([node(N,LBT,RBT) | T],[N|Result]):- append([LBT,RBT],T,Z),preorderHelper(Z,Result).

trPreorder(empty,[]).
trPreorder(Node,L):- preorderHelper([Node],Result), append([],Result,L).

inorderHelper(empty,[],[]).
inorderHelper(empty,[node(N,_LBT,RBT)|T],[N|Result]):- inorderHelper(RBT,T,Result).
inorderHelper(node(N,LBT,RBT),T,Result):- append([node(N,LBT,RBT)],T,Z),inorderHelper(LBT,Z,Result).

trInorder(empty,[]).
trInorder(Node,L):- inorderHelper(Node,[],Z), append([],Z,L).

reverseList([],X,X). % here X is the resultant list.
reverseList([H|T],X,Z):- append([H],Z,W),reverseList(T,X,W).
postorderHelper([],[]).
postorderHelper([node(N,empty,empty)|T],[N | Result]):- postorderHelper(T,Result).
postorderHelper([node(N,LBT,empty)|T],[N | Result]):-append([LBT],T,Z),postorderHelper(Z,Result).
postorderHelper([node(N,empty,RBT)|T],[N | Result]):-append([RBT],T,Z),postorderHelper(Z,Result).
postorderHelper([node(N,LBT,RBT)|T],[N | Result]):- append([RBT,LBT],T,Z),postorderHelper(Z,Result).

trPostorder(empty,[]).
trPostorder(Node,L):- postorderHelper([Node],X),reverseList(X,Result,[]),append([],Result,L).


% Since eulerTour of a Tree can be obtained by the corresponding subtrees recursively
% So we will do it recursively.
eulerTour(empty,[]).
eulerTour(node(N,LBT,RBT),L):- eulerTour(LBT,X),eulerTour(RBT,Y),append([N],X,A),append(A,[N],B),append(B,Y,C),append(C,[N],L).

% We can get the preorder traversal and post order traversal by adding list element 1 by 1 of
% eulertour in a way that no repeated elements are present,and as for the postorder traversal
% we can do the same from the back side of eulerTour and for the preorder we do it from the front side.
removeDups([], []).
removeDups([H | T], Result) :- member(H, T), removeDups(T, Result).
removeDups([H | T], [H | Result]) :- not(member(H, T)), removeDups(T, Result). 

preET(BT,L):- eulerTour(BT,X),reverseList(X,Rev,[]),removeDups(Rev,Y),reverseList(Y,L,[]).
inET(BT,L):- eulerTour(BT,_X),inorder(BT,L).
postET(BT,L):- eulerTour(BT,X),removeDups(X,L).


%toString function can be made by recursive calling
toString(empty,"()").
toString(node(N,LBT,RBT),S):-toString(LBT,S1),toString(RBT,S2),
                string_concat("(",N,A),string_concat(A,", ",B),string_concat(B,S1,C),
                string_concat(C,", ",D),string_concat(D,S2,E),string_concat(E,")",S).


% For isBalanced all we need to do is recursively compare the difference of subtree heights.
%absolute defined for check if height difference in range
absolute(1).
absolute(-1).
absolute(0).

isBalanced(empty).
isBalanced(node(_N,LBT,RBT)):- height(LBT,X),height(RBT,Y),Z is X - Y,absolute(Z),
                            isBalanced(LBT),isBalanced(RBT).


%For checking if a Binary tree is BST or not,we will do the inorder traversal 
% And check if the result obtained is in sorted order or not.
sorted([]).
sorted([_]).
sorted([X,Y| T]):- Y >= X,sorted([Y|T]).
isBST(BT):- inorder(BT,L),sorted(L).

% For makeBST first we will make a rule which will take a sorted list and then
% produce a bst from it by recursively dividing the list in which
% we will stop when either, empty list is there then bst will be 
% (empty), or when there is only 1 element in the list,then bst
% will be (node(N,empty,empty)).
%Step 1 is to get a way to partition a sorted list
divideList([],[A],[A]).
divideList([A],[B],[A,B]).
divideList(List1,List2,Final):- append(List1,List2,Final),length(List1,N),length(List2,N).
divideList(List1,List2,Final):- append(List1,List2,Final),length(List1,N),X is N +1,length(List2,X).
% Now we make a helper predicate to make bst from sorted list.
helpBST([],empty).
helpBST([N],node(N,empty,empty)).
helpBST(L,node(N,LBT,RBT)):-divideList(A,[N|T],L),helpBST(A,LBT),helpBST(T,RBT).

makeBST(L,BST):- sort(L,X),helpBST(X,BST).


% For lookup simply see if N is a member of the inorder traversal or not
lookup(N,BST):- inorder(BST,L),member(N,L).


% For insert we will simply compare the root and the inserting value
% if it is less, we insert in the left subtree else right subtree
helpInsert(N,empty,node(N,empty,empty)).
helpInsert(N,node(X,LBT,RBT),node(X,L,RBT)):- N < X,helpInsert(N,LBT,L).
helpInsert(N,node(X,LBT,RBT),node(X,LBT,R)):- N >X, helpInsert(N,RBT,R). 

insert(N,BST1,BST2):- not(lookup(N,BST1)), helpInsert(N,BST1,BST2).


% For delete we will simply remove the root if it is the required node and
% check for if the left or right subtree is nil or we can just compare the
% values and then recurse in the subtrees
helpRemove(N,node(N,empty,empty),empty).
helpRemove(N, node(N, L, empty), L).
helpRemove(N, node(N, empty, R), R).
helpRemove(N,node(X,LBT,RBT),node(X,L,RBT)):- N < X,helpRemove(N,LBT,L).
helpRemove(N,node(X,LBT,RBT),node(X,LBT,R)):- N >X, helpRemove(N,RBT,R). 
delete(N,BST1,BST2):- lookup(N,BST1), helpRemove(N,BST1,BST2).
