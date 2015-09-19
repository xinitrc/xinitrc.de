---
title: Magic tricks
tags: haskell, graphs, semiring
---

After in the last [blog post](/blog/2014/02/09/Sucker-for-generality.html) I described some algebraic strutures and already said I would like to show case how to solve some very different problems with those. I think it's now time to go into that. I start with something which is more or less a reiteration of a fantastic [blog post](http://r6.ca/blog/20110808T035622Z.html) by Russell O'Conner on mostly graph problems. I will layout the problems a little differently and hope to convey the ideas behind the post to a wider audience. I will also omit some stuff and maybe come back to that in a later post. But now without further ado let's get started.

<!--more-->

First up, I had another blog post in mind at first, which in retrospect seemed a little to complicated for a first post. Since I changed the topic of this post slightly, I had to go back to the [last post](/blog/2014/02/09/Sucker-for-generality.html) and add another algebraic structure the \(*\)-semiring. 

## Square Matrixes

So after introducing the definition of a (\(*\)-)semiring in the last post, let's now look at some examples. First let's agree that any square matrix over a semiring is a semiring as well. To do that we first need to define the two operations \(\oplus\) and \(\otimes\) in terms of the operations on the underlying semiring. In other words given a semiring \((X, \oplus, \otimes, \mathbf{0}, \mathbf{1})\) give a semiring \((X^{\mathbb{N}\times\mathbb{N}}, \oplus_{m}, \otimes_{m}, \mathbf{0}_{m}, \mathbf{1}_{m})\) such that this also forms a semiring. The second part is to select the elements  \(\mathbf{0}_{m}\) and \(\mathbf{1}_{m}\).

Actually this is simple for the \(\oplus_m\) Operation we just have to lift the \(\oplus\) operation of our underlying semiring to matrixes. Lifting simply means doing the operation \(\oplus\) on every component of the matrix. So the first item in the first row of both matrixes are connected with \(\oplus\) then the second items of the first rows and so on and so on. Writtten mathematically:

\[C = A \oplus_{m} B \Leftrightarrow c_{ij} = a_{ij} \oplus b_{ij}\]

here \(a_{ij}\) simply means the element at the ith column in the jth row of matrix \(A\).
 
Ok now for the second operation \(\otimes_m\) we simply use the same "trick" we usually us to multiply two matrixes, we use the \(\otimes\) operation component wise on the rows of the first matrix with the corresponding columns of the second matrix and than use the \(\oplus\) operation over all of these results to get one result. Or mathmatically put:

\[C = A \otimes_{m} B \Leftrightarrow c_{ij} = \oplus_{k=0}^{n} (a_{kj} \otimes b_{ik})\]

where \(n\) is the size of the matrix.

What remains open are the neutral elements, but these both are easy. To do nothing to any component we can add \(\mathbf{0}\) from the underlying semiring, since we already know this doesn't change anything, due to the rules of the underlying semiring, it wont change anything in the square matrixes. So \(\mathbf{0}_m\) is just a square matrix consisting of all compontents \(\mathbf{0}\). 

For the \(\mathbf{1}_{m}\) we use the identity matrix so the matrix, where all elements on the main diagonal are 1, we only have to use the \(\mathbf{1}\) from our semiring, and everything works out perfectly.

Now you have only to suffer through the next section for an implementation, which is somewhat taylored to our application.

## Graphs 

One application of square matrixes is to describe the connections of graphs. Actually depending on your definition of a graph the matrix can be used as a complete definition of the graph, so let's do so. First we need a type to hold the edges. Since we don't want to specify what we can put in those edges, this will later help us to use one representation of graphs and edges for multiple purposes. 

~~~~ {.haskell}
data Edge a = a :-> a deriving (Eq, Ord, Bounded, Ix)

~~~~

Here we have an Edge datatyp with a constructor <span class="tt">:-&gt;</span>. And we let the Haskell compiler figure out how we can check for Equality (**Eq**), that we can order edges (**Ord**), that there is a min and a maximum bound (**Bounded**) on all the types and that we can Index (**Ix**) especially the last two will be very useful later on.

Now we come to our matrix type. For a Matrix we need two types of input parameters, the type of index data, and the type of the content. The index data tells us how to address the components. In Haskell we could use any type for this as long as we have some mapping to the integers. This is a little more flexible in writing than always having to switch bewteen your mental model and if it is [x][y] or [y][x]. 

In addition we say that Matrixes are an instance of Applicative, which denotes that it is an applicative functor. I wont go into this in this post just think of it as it implements the container interface and we can apply functions to the individual components. This is not perfectly acurate but for the moment it will do. 

Third we add two helper function, one which generates a list with one entry for every component of the resulting matrix, and one which takes a function then takes every component of our matrix and applys this function. These two functions simply do the following, whenever we want to create a new square matrix for a specific type we generate a dummy element for every entry and then apply a function to convert that dummy element into the final entry. In Java or C you would have a for loop iterating through the numbers 0 up to n (this is the dummy list) and then do something like <span class="tt">x[i] = f(i)</span> where f is some function in the body of the for loop.

Last but not least we say square matrixes over edges where the content type is a semiring are as well a semiring, with the operations just like I told you before.

~~~~ {.haskell}
newtype Matrix i e = Matrix {unmatrix :: (Array (Edge i) e)}

instance (Ix i) => Functor (Matrix i) where
  fmap f (Matrix m) = Matrix (fmap f m)

instance (Ix i, Bounded i) => Applicative (Matrix i) where
  pure x = matrix (const x)
  Matrix f <*> Matrix x = matrix (\(i :-> j) -> (f!(i :-> j)) (x!(i :-> j)))
	  
entireRange :: (Ix i, Bounded i) => [i]                                                                             
entireRange = range (minBound, maxBound)                                                                                            
matrix :: (Ix i, Bounded i) => (Edge i -> e) -> Matrix i e
matrix f = Matrix . listArray (minBound, maxBound) . map f $ entireRange
	  
instance (Ix i, Bounded i, Semiring a) => Semiring (Matrix i a) where
  zero = pure zero
  one = matrix (\(i :-> j) -> if i == j then one else zero)
  (<+>) = liftA2 (<+>)
  (Matrix x) <.> (Matrix y) = matrix build
      where
	      build (i :-> j) = foldr (<+>) zero [x!(i :-> k) <.> y!(k :-> j) | k <- entireRange]
						
~~~~ 


So now we come to the heart and soul of this blog post, a mere 7 lines of code.

~~~~ {.haskell}
instance (Ix i, Bounded i, StarSemiring a) => StarSemiring (Matrix i a) where
  plus x = foldr f x entireRange
    where
	  f k (Matrix m) = matrix build
	    where
		  build (i :-> j) = m!(i :-> j) <+>
                            m!(i :-> k) <.> star (m!(k :-> k)) <.> m!(k :-> j)
														  
~~~~

Ok so what does it do. It stipulates that matrixes over a star semiring are also star semirings. For that we need to implement one of the two operations \(*\) or \(+\). Since \(*\) will lead us to a infinite recursion I opted for \(+\), but what does it do? First up a recap the operations of a \(*\)-semiring are Fixpoint operators, that is the performe one or more operations until there is no more change (or ad infinitum if there is always progress). So how would we implement this.

Let's formulate this in terms of graphs and their connectedness structure, we start with an initial setup, and then go through every node <span class="tt">foldr f x entireRange</span> and take a look at every component if either the current result doesn't change (<span class="tt">m!(i:->j)</span>) or (<span class="tt">&lt;+&gt;</span>) if we can get a "better" result using the new node <span class="tt">k</span> (<span class="tt"> m!(i :-> k) &lt;.&gt; star (m!(k :-> k)) &lt;.&gt; m!(k :-> j)</span>).

I give you that sounds really vague, how can this be of any significance. This is where the fun starts, we use different underlying \(*\)-Semirings. Let's start with the most simple one.

## Boolean \(*\)-Semiring

The boolean \(*\)-Semiring is really simple, first the semiring-part, I will use the standard programming notation for **and** and **or** to make it a little easier. Than this \((\{0,1\}, ||, \&\&, 0, 1)\) is the Boolean semiring. For this one I will do a little sanity check for the later semirings I let you do that on your own. First if I take any boolean (0 or 1) and do an or operation with 0 we will get the original value back. <span class="tt">(0 || 0 = 0, 1 || 0 = 1, 0 || 1 = 1)</span>. Same goes for the and operation and 1 <span class="tt">(0 && 1 = 0, 1 && 0 = 0, 1 && 1 = 1)</span>. These exaustiv examples also take care of the property that <span class="tt">a || b = b || a</span>. What remains to show is that the operations are distributing over one another, but this is what is called [Rule of replacement](https://en.wikipedia.org/wiki/Distributive_property). So we have established that booleans form a semiring. 

Now for the \(*\) part, this is simple (and somewhat unceremonial) too. Looking at the definition of \(*\) we have: 

\[x^{*}=1\oplus x\otimes x^*\]

So let's put the operators from our boolean semiring into place. Then this becomes:

\[x^{*}=1 || x \&\& x^*\] 

now we simply use short circute evaluation the result of 1 || anything will always be 1 so we can simply say \(x^*=1\) for any \(x\) and now we have a star semiring. 

### Haskell 

In Haskell this is equally simple

~~~~ {.haskell}
instance Semiring Bool where
  zero = False
  one = True 
  (<+>) = (||)
  (<.>) = (&&)

instance StarSemiring Bool where
  star x = one
~~~~

### Application

So now let's put this to a good use. Since we now have a \(*\)-semiring we know every square matrix over this also forms a \(*\)-semirings.  For an example take the following graph as an example 

§tikz(->,>=stealth',shorten >=1pt,auto,node distance=2.8cm, semithick)§
  \tikzstyle{every state}=[text=black]
 \node[state] (N5) {N5};
 \node[state] (N3) [below of=N5]{N3};
 \node[state] (N6) [left of=N3]{N6};
 \node[state] (N4) [right of=N3]{N4};
 \node[state] (N1) [below left of=N3]{N1};
 \node[state] (N2) [below right of=N3]{N2};
 
 \path[->](N5) edge (N6)
          (N6) edge (N3)
		  (N6) edge (N1)
		  (N1) edge (N3)
		  (N1) edge (N2)
		  (N2) edge (N4)
		  (N3) edge (N2)
		  (N3) edge (N4)
		  (N4) edge (N5);
§endtikz§

We can represent this as a matrix, we have a row and column for every node in the graph and whenever there is an edge from one node to another, we have a 1 in the row of the origin at the column of the target node. This looks as follows 

\[C=\left (\begin{array}{cccccc}
0 & 1 & 1 & 0 & 0 & 0\\
0 & 0 & 0 & 1 & 0 & 0\\
0 & 1 & 0 & 1 & 0 & 0\\
0 & 0 & 0 & 0 & 1 & 0\\
0 & 0 & 0 & 0 & 0 & 1\\
1 & 0 & 1 & 0 & 0 & 0\\ 
\end{array}\right)\]

In Haskell we can put like this. 

~~~~ {.haskell}
type Graph i = Matrix i Bool

graph :: (Ix i, Bounded i) => [Edge i] -> Graph i
graph edgeList = matrix build
  where
      build i = i `elem` edgeList
	  
data Nodes = N1 | N2 | N3 | N4 | N5 | N6 deriving (Eq, Ord, Bounded, Ix, Show)

exampleGraph1 = graph [N1 :-> N2, N1 :-> N3, N2 :-> N4, N3 :-> N2, N3 :-> N4, N4 :-> N5, N5 :-> N6, N6 :-> N1, N6 :-> N3]
~~~~

So ok, now we have this in Haskell, the question is what does our \(*\) operation for Matrixes do in this case. Let's review that: 

For every entry component of the matrix we go through every node of the matrix and check if we can get a "better" result by taking that node. 

Let's do this in an example. Take the entry in the first row of at the fourth column. <span class="tt">m!(N1 :-&gt; N4)</span> in our initial matrix it is 0. Now let's go trough every node and do 

<span class="tt">m!(i :-&gt; j) &lt;+&gt; m!(i :-&gt; k) &lt;.&gt; star (m!(k :-&gt; k)) &lt;.&gt; m!(k :-&gt; j)</span>. 

We first rewrite this to use our semiring then we get 

<span class="tt"> m!(i :-> j) || m!(i :-&gt; k) && star (m!(k :-&gt; k)) && m!(k :-&gt; j)</span>. 

So when we go through every node N1 upto N6 the following happens:
<ol>
<li><span class="tt"> m!(N1 :-> N4) || m!(N1 :-&gt; N1) && star (m!(N1 :-&gt; N1)) && m!(N1 :-&gt; N4)= 0 || 0 && star (0) && 0=0|| 0 && 1 && 0=0</span></li>
<li><span class="tt"> m!(N1 :-> N4) || m!(N1 :-&gt; N2) && star (m!(N2 :-&gt; N2)) && m!(N2 :-&gt; N4)= 0 || 1 && star (0) && 1=0|| 1 && 1 && 1=1</span></li>
<li><span class="tt"> m!(N1 :-> N4) || m!(N1 :-&gt; N3) && star (m!(N3 :-&gt; N3)) && m!(N3 :-&gt; N4)= 1 || 1 && star (0) && 1=1|| 1 && 1 && 1=1</span></li>
<li><span class="tt"> m!(N1 :-> N4) || m!(N1 :-&gt; N4) && star (m!(N4 :-&gt; N4)) && m!(N4 :-&gt; N4)= 1 || 1 && star (0) && 0=1|| 1 && 1 && 0=1</span></li>
<li><span class="tt"> m!(N1 :-> N4) || m!(N1 :-&gt; N5) && star (m!(N5 :-&gt; N5)) && m!(N5 :-&gt; N4)= 1 || 0 && star (0) && 0=1 || 0 && 1 && 0=1</span></li>
<li><span class="tt"> m!(N1 :-> N4) || m!(N1 :-&gt; N6) && star (m!(N6 :-&gt; N6)) && m!(N6 :-&gt; N4)= 1 || 0 && star (0) && 0=1|| 0 && 1 && 0=1</span></li>
</ol>

Now what happend here? As soon as we hit the second iteration we found a path from N1 to N4 via N2 and the entry in the matrix changed from 0 to 1. If we do this for all entries what will happen is, we calcuate the transitive connection relation, in other words we calculate which node we can reach from which other node via any path.

Without all the fuzz, what we did here is implementing the so called Warshall-Algorithm in Haskell, in a somewhat complicated way I give you that. But the nice part is, we are not done here. 

## Tropical \(*\)-semiring

In the last post I already introduced the Tropical \(*\)-Semiring \((\mathbb{N}\cup \{\infty\}, min(x,y), +, \infty, 0)\), the \(*\) is in the update.

### Application

Let's update our example slightly, we now make the edges go both ways and attach weights to them. You can think of those as distances. 

§tikz(--,>=stealth',shorten >=1pt,auto,node distance=2.8cm, semithick)§
  \tikzstyle{every state}=[text=black]
 \node[state] (N5) {N5};
 \node[state] (N3) [below of=N5]{N3};
 \node[state] (N6) [left of=N3]{N6};
 \node[state] (N4) [right of=N3]{N4};
 \node[state] (N1) [below left of=N3]{N1};
 \node[state] (N2) [below right of=N3]{N2};
 
 \path[--](N5) edge node {9} (N6)
          (N6) edge node {2} (N3)
		  (N6) edge node {14} (N1)
		  (N1) edge node {9} (N3)
		  (N1) edge node {7} (N2)
		  (N2) edge node {15} (N4)
		  (N3) edge node {10} (N2)
		  (N3) edge node {11} (N4)
		  (N4) edge node {6} (N5);
§endtikz§

Or as a matrix we can represent this like this. 

\[C=\left (\begin{array}{cccccc}
\infty  &  7 &  9 &  \infty &  \infty & 14\\
7  &  \infty & 10 & 15 &  \infty &  \infty\\
9  & 10 &  \infty & 11 &  \infty &  2\\
\infty  & 15 & 11 &  \infty &  6 &  \infty\\
\infty  &  \infty &  \infty &  6 &  \infty &  9\\
14 &  \infty &  2 &  \infty &  9 &  \infty\\ 
\end{array}\right)\]

Or in Haskell

~~~~ {.haskell}

exampleEdgeList2 :: (Edge Node2) -> Maybe Integer
exampleEdgeList2 (i :-> j) = (lookup (i :-> j) edges) `mplus` (lookup (j :-> i) edges)
  where
      edges = [(N1 :-> N2, 7), (N1 :-> N3, 9), (N1 :-> N6, 14),
	           (N2 :-> N3,10), (N2 :-> N4,15),
			   (N3 :-> N4,11), (N3 :-> N6, 2),
			   (N4 :-> N5, 6),
			   (N5 :-> N6, 9)]
														  
~~~~

Now what does our \(*\) operation do now? Let's go with the same example we did above, the first row, fourth column.

We once again go through N1 to N6 in search for what happnes to N1 :-> N4

<ol>
<li><span class="tt"> min(m!(N1 :-> N4), m!(N1 :-&gt; N1) + star (m!(N1 :-&gt; N1)) + m!(N1 :-&gt; N4)= min(&infin;,  &infin; + star (&infin;) + &infin;)= min(&infin;, &infin; + 0 + &infin;)=&infin;</span></li>
<li><span class="tt"> min(m!(N1 :-> N4), m!(N1 :-&gt; N2) + star (m!(N2 :-&gt; N2)) + m!(N2 :-&gt; N4)= min(&infin;,  7 + star (&infin;) + 15)= min(0, 7 + 0 + 15)=22</span></li>
<li><span class="tt"> min(m!(N1 :-> N4), m!(N1 :-&gt; N3) + star (m!(N3 :-&gt; N3)) + m!(N3 :-&gt; N4)= min(22,  9 + star (&infin;) + 11)= min(22, 9 + 0 + 11)=20</span></li>
<li><span class="tt"> min(m!(N1 :-> N4), m!(N1 :-&gt; N4) + star (m!(N4 :-&gt; N4)) + m!(N4 :-&gt; N4)= min(20,  20 + star (&infin;) + &infin;)= min(20, 20 + 0 + &infin;)=20</span></li>
<li><span class="tt"> min(m!(N1 :-> N4), m!(N1 :-&gt; N5) + star (m!(N5 :-&gt; N5)) + m!(N5 :-&gt; N4)= min(20,  &infin; + star (&infin;) + &infin;)= min(20, &infin; + 0 + &infin;)=20</span></li>
<li><span class="tt"> min(m!(N1 :-> N4), m!(N1 :-&gt; N6) + star (m!(N6 :-&gt; N6)) + m!(N6 :-&gt; N4)= min(20,  14; + star (&infin;) + &infin;)= min(20, 14 + 0 + &infin;)=20</span></li>
</ol>

So what happend here, on step 2 we got a new value for our entry in the matrix, which at step three was again changed. What you can see here, is the same algorithm instantiated with another semiring calculates the shortest distance between the nodes. 

Oh this algorithm by the way is known as the Floyd- or [Floyd-Warshall-Algorithm](https://en.wikipedia.org/wiki/Floyd-Warshall). I doubt that at it's conception the semiring property of bool and the tropical semiring was properly observed. You can obviously come from the Warshall- to the Floyd-Warshall-Algorithm by just looking long enough at the original code I believe.

Let's do two more semirings. 

## MaxMin \(*\)-semiring

Another semiring we could think up is this \((\mathbb{N}\cup\{\infty\}, max(x,y), min(x,y), 0, \infty)\). I have trust in you that you can check that this ins indeed a semiring on your own. For the \(*\) operation I give you some help, even though this is easy too. Once again the definining property is:

\[x^*=\mathbf{1}\oplus x\otimes x^*\]

which in this case means:

\[x^*=max(\infty, min(x,x^*))\]

and once again short circut evaluation yields simply that \(x^*=\infty\) since the maximum of \(\infty\) and anything else will be \(\infty\).

### Application
I wont go through the algorithm again, but just go through it on a more thought experimental way. 

<span class="tt"> max (m!(i :-> j),  min (min (m!(i :-&gt; k), star (m!(k :-&gt; k)), m!(k :-&gt; j))</span>

This is the line of our algorithm with the operations from the semiring given above. Let's examin what this does. For every connection from one node to another we take the maximum of all possible pathes connecting two nodes, but take the minimum of all the connections in between. So if we have two pathes leading from one node to another we take the one where the smallest edge weight is larger than the smallest edge weight on the other path. 

Assume that the weights now represent throuhgput through some channel, than this algorithm gives us the channel to choose to send the most data in the shortest time. In our examplegraph from above for example we would take the path \(N1 \rightarrow N3 \rightarrow N4\) over \(N1\rightarrow N2\rightarrow N4\) because the minimum edge weight on the first path is 9 while on the second path it is 7. 

I will only give you the semiring for MaxMin here and will give a full Haskell file with everything implemented later.

~~~~ {.haskell}

data MinMax = Infty | MinMax Integer deriving (Eq, Ord)

instance Semiring MinMax where
  zero = MinMax 0
  one = Infty
  _ <+> Infty = Infty
  Infty <+> _ = Infty
  (MinMax a) <+> (MinMax b) = MinMax $ max a b
  Infty <.> x = x
  x <.> Infty = x
  (MinMax a) <.> (MinMax b) = MinMax $ min a b
			
instance StarSemiring MinMax where
  star _ = one
~~~~


## MaxMult \(*\)-semiring

So final example for this blog post. Another semiring, this time \(([0,1], max(x,y), \cdot, 0, 1)\). This time I fully trust you that you can check that this is a semiring and that you can find a suitable \(*\) operation.

### Application 

Let's assume we have a graph like the one above but this time the edge weights are real values between 0 and 1. We interprete these as reliabilities. So the higher the number the more reliable the path. Should you want to send a data package from one node to another you would obviously like to have it go the most reliable path. And that is exactly what the \(*\) operation will now do. 

Once again taking the significant line from our algorithm and substituting the operations in we get this.

<span class="tt"> max (m!(i :-> j), (m!(i :-&gt; k) * star (m!(k :-&gt; k)) * m!(k :-&gt; j))</span>

So on every path we multiply the weights, giving us the overall reliability of that path, and then take the path with the highest such value. 

What follows is the Haskell implementation of the Semiring, I will give you a Haskell file containing everything a little later.

~~~~ {.haskell}
data Reliability = Reliability Double deriving (Eq, Ord)

instance Semiring Reliability where
  zero = Reliability 0
  one = Reliability 1
  (Reliability a) <+> (Reliability b) = Reliability $ max a b
  (Reliability a) <.> (Reliability b) = Reliability $ a * b
		
instance StarSemiring Reliability where
  star _ = one
		  
~~~~


## Wrapping up

What I showed you are 7 very powerful lines of code, implementing a very general algorithm. Just from this blog post they implement the transitive connection relation, length of the shortest path between two nodes, the maximum throughput between two nodes and the highest possible reliability for a path. Currently they don't give you the actual pathes achieving those properties, but, probably not much to your surprise, we can actually do that with our newest best friend the \(*\)-semiring too. But since this blog post is long enough already I will leave that for the next blog post. What remains is [this link](/assets/documents/Semiring1.hs) to the file containing a working implementation and some examples.  
