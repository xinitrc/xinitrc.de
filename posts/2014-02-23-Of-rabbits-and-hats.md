---
title: Of rabbits and hats
tags: semiring, algebra, haskell, graphs
bgimage: /assets/images/hats.jpg
position: tl
credit: arbyreed
crediturl: https://www.flickr.com/photos/19779889@N00/
---

In the last blog post I showed you what you can do with just implementing the interface for a \(*\)-semiring and than using the matrix over that semiring. Up until now, we can compute the transitive connection relation, the length of the shortest path, the maximum throughput between two nodes and the reliability on the the most reliable path. But what we can't do right now is producing the path(s) that instatiate these properties. So this is exactly what I will show you in this post.

<!--more-->

## Regular Expressions

As you might have to expected, especially since I wrote it in the last blog post, we will use our new best friend the \(*\)-semiring to do so. We acutally come to the premier example of a semiring, the regular expression. You migth know regular expressions from other programming languages, which is as helpful as it is unfortunate. Why is that you might ask and the answer is simple: There is a singificant difference between regular expressions used in theoretical computer science and regular expressions as implemented in programming languages. The later are strictly more powerful. The nice part is they are also a syntactical super set of the first and by simply dropping this extra syntax we get exactly the former. So for you, this might result in some unlearning, I can't help that, sorry.

So for everybody not knowing regular expressions from computer science curriculum, let's give a briefe explanation. 

First we need a finite set of "***letters***" called ***alphabet*** usually for illustrative purposes one uses the regular letters of a usual alphabet, like \(a, b, c, d, \ldots\). But any alphabet will do, we could for example use the natural numbers up to some number, the edges of a finite graph or all subsets of \(\{1,2,3,4\}\) as letters, it doesn't matter. 

Let \(l\) be any letter of the alphabet \(A\), then we can give the following recursive definition of the regular expressions \(re\) as follows:

\[re,re_1, re_2 ::= \emptyset | \epsilon | l | (re_1 + re_2) | (re1 \cdot re2) | (re^{*})\] 

Let's go through this and decipher what it means. 

1. \(\emptyset\) this is simple. If we write nothing, it is a valid regular expression. And since we can't leave blank space in a definition and think that everybody picks up that this is a significant syntactical element we write this as \(\emptyset\).
2. \(\epsilon\) we need to construct a word that has no letters, to do that we use the symbol \(\epsilon\). You might ask what is the difference between nothing and a a word of no letters. It is the same difference as between \({0}\) and \(\emptyset\) one is a set containing something of no value, the other is simply empty, same goes here.
3. If we have a letter \(l\), writing just that letter is a valid regular expression.
4. \(re_1 + re_2\) If we have two regular expressions already, we can either use the left or the right, appropriatly this is usually called **alternative** or **choice**. The plus-sign is somewhat of a convention for regular expressions, but you might have already guessed what will make it's way into our semiring in the end.
5. \(re_1 \cdot re_2\) this is called **concatenation** or **sequential compositon** it simply states that we can look for something that matches the first regular expression followed by something that matches the second regular expression. If there is no danger of misreading it we usually ommit the \(\cdot\) just like with multiplication.
6. \(re^*\) this finally is arbitrary iteration, if we have a regular expression we can match it an arbitrary amount of times.

Usually you define some form of rules that say we can ommit some of the parens by saying \(*\) binds stronger than \(\cdot\) which in turn binds stronger than \(+\). Since this just looks like basic arithmetic I trust you can follow.

### Example
Let's give an example, suppose we have this regular expression: 

\[re_{example}=a (bd + ce)\]

Then let's first check that it is a valid regular expression for the alphabet \(\{a,b,c,d,e\}\). After stating this, we know that the individual letters \(a,b,c,d\) are valid regular expressions. Next we combine \(b\) and \(d\) with \(\cdot\) and get \((b\cdot d)\) same for \(c\) and \(e\) now we can apply \(+\) and get \((b\cdot d+c\cdot e)\). As a last step we combine \(a\) and \((b\cdot d+c\cdot e)\) and get \(a\cdot (bd+ce)\). Ommiting the \(\cdot\)'s we get that the regular expression is valid. 

#### Now what does it "mean"

Let's assume we have a graph and the edges are labeled with letters so there is an edge between two nodes labeld with \(a\), one labeled \(b\) and so on, like in this graph.

§tikz(->,>=stealth',shorten >=1pt,auto,node distance=2.8cm, semithick)§
  \tikzstyle{every state}=[text=black]
 \node[state] (N1) {N1};
 \node[state] (N2) [right of=N1]{N2};
 \node[state] (N3) [right of=N2]{N3};
 \node[state] (N4) [above of=N2]{N4};
 \node[state] (N5) [right of=N4]{N5};
 
 \path[->](N1) edge node {a} (N2)
          (N2) edge node {b} (N3)
		  (N2) edge node {c} (N4)
		  (N3) edge node {d} (N5)
		  (N4) edge node {e} (N5);
§endtikz§

Then the regular expression above \(a(bd+ce)\) describes all pathes from \(N1\) to \(N5\). We first have to take the edge labeled \(N1\rightarrow N2\) which is labeld \(a\) then either the edges labeled \(b\) followed by the edge labeled \(d\) or the edges labeled \(c\) and \(e\).

I think you see how this is useful for our case. So let's go on and define a semiring.

## The Regular Expression \(*\)-Semiring

Acutally regular expressions form a family of \(*\)-semirings, one for each underlying alphabet. So let's call our alphabet \(\Sigma\), which is the usual name for the alphabet in computer science. Then 

\[(\Sigma, +, \cdot, \emptyset, \epsilon)\] is a \(*\)-semiring with the rules above and two corner cases which give rise to the following two (standard) definitions:

1. \(\emptyset + x = x + \emptyset = x\)
2. \(\emptyset \cdot x = x \cdot \emptyset = \emptyset\)

Let's do a quick check if our properties hold. 

1. \(a\oplus b = b\oplus a\) since \(+\) is the same as **or** this holds.
2. \((a\oplus b)\oplus c = a\oplus (b\oplus c)\), by almost the same argument, if we first decide between \(a\) or \(b\) and then between the result of that decission or \(c\) it's the same as deciding the other way around.
3. \(a \oplus \mathbf{0}=\mathbf{0}\oplus a=a\), this is by the definition given above. 
4. \((a \otimes b)\otimes  c = a \otimes  (b \otimes c)\) the concatenation of \(a\) and \(b\) yields \(ab\) that concatenated with \(c\) yields \(abc\) which is the same as first concatenating \(b\) and \(c\) to \(bc\) and then prepending \(a\).
5. \(a \otimes \mathbf{1} = \mathbf{1} \otimes  a = a\) concatenating the empty word to anything will not change anything, so this is ok too.
6. \(a \otimes  \mathbf{0} = \mathbf{0} \otimes  a = \mathbf{0}\) that's by the definition above. 
7. \(a \otimes (b \oplus c) = (a\otimes b) \oplus (a\otimes b)\), this is simply either first doing \(a\) and then deciding between \(b\) or \(c\) or first deciding to go \(ab\) or \(ac\) so this holds true to.
8. \((a \oplus b) \otimes c = (a\otimes c) \oplus (b\otimes  c)\), almost the same as the one before, either decided between \(a\) and \(b\) and then do \(c\) or deciding to do \(ac\) or \(bc\) should be the same. 

Ok, we are satisfied, this is a semiring. Now for the \(*\) part, which this time is a little more interesting than before. 

If we want to describe all ways from one node to another, there might be loops on the way. For example let's modify the graph from above slightly that it looks like this.

§tikz(->,>=stealth',shorten >=1pt,auto,node distance=2.8cm, semithick)§
  \tikzstyle{every state}=[text=black]
 \node[state] (N1) {N1};
 \node[state] (N2) [right of=N1]{N2};
 \node[state] (N3) [right of=N2]{N3};
 \node[state] (N4) [above of=N2]{N4};
 \node[state] (N5) [right of=N4]{N5};
 
 \path[->](N1) edge node {a} (N2)
          (N2) edge node {b} (N3)
		  (N2) edge node {c} (N4)
		  (N3) edge node {d} (N5)
		  (N4) edge [loop above] node {f} (N4)
		  (N4) edge node {e} (N5);
§endtikz§

Now on a way from \(N1\) to \(N5\) we would be allowed to take the loop labeled \(f\) arbitrarily often, or writing it in the syntax of regular expression \(a(bd+cf*e)\). We have to take this into account for our \(*\)-semiring definition of regular expressions. So let's do it.

### Defining \(*\)s

I have a small problem since now we have two different \(*\) operations, one from the semiring and one from the regular expressions. To keep those apart I will use \(*_{sr}\) for the semiring and \(*_{re}\) for the regular expression star. I hope this isn't to confusing for you. 

Let us define a \(*\). 

\[x^*_{sr}=\left\{\begin{array}{ll}\epsilon & \text{falls } x = \emptyset\\\epsilon & \text{falls } x = \epsilon\\y^{*_{sr}} & \text{falls } x = y^{*_{re}}\\x^{*_{re}} & \text{sonst}\end{array}\right.\]

Even though this is a bit tricky I think you are by now fully capable of checking that this will actually give us a valid \(*\)-semiring. 

### Haskell 

Now for a Haskell implementation. Since I don't want to conflict with other operators I use <span class="tt">Or, Concat</span> and <span class="tt">Star</span> instead of \(+, \cdot\) and \(*\). 

~~~~ {.haskell}
data StarSemiringExpression a = 
    Var a 
  | Or (StarSemiringExpression a) (StarSemiringExpression a)
  | Concat (StarSemiringExpression a) (StarSemiringExpression a)
  | Star (StarSemiringExpression a)
  | None 
  | Empty

newtype RE a = RE (StarSemiringExpression a)

re :: a -> RE a
re = RE . Var

instance Semiring (RE a) where
  zero = RE None
  one = RE Empty
  RE None <+> x = x
  x <+> RE None = x
  RE Empty <+> RE Empty = RE Empty
  RE Empty <+> RE (Star a) = RE (Star a)
  RE (Star a) <+> RE Empty = RE (Star a)
  RE x <+> RE y = RE (x `Or` y)
  RE Empty <.> x = x
  x <.> RE Empty = x
  RE None <.> _ = RE None
  _ <.> RE None = RE None
  RE x <.> RE y = RE (x `Concat` y)
  
instance StarSemiring (RE a) where
  star (RE None) = RE Empty
  star (RE Empty) = RE Empty
  star (RE (Star x)) = star (RE x)
  star (RE x) = RE (Star x)
~~~~

The helper function <span class="tt">re</span> in combination with leaving out the actual alphabet for our regular expression allows us to use any type, as I've shown above, and implicitly creating the alphabet from the "letters" that are used in the regular expression. (For mathmatically inclined readers: Yes, this is only ok, as long as we only use finite regular expression, but I would doubt that you can write an infinite one ;-) )

There is one thing left to do to put this to use, we need another small helper function: 

~~~~ {.haskell}
reGraph :: (Ix i) => Matrix i (Maye a) -> Matrix (Re a) 
reGraph = fmap (maybe zero re)
~~~~ 

This simply takes a matrix, where there might be an entry at any index and transforms that into a regular expression of one letter and takes the entry itself as letter, if there is no entry present at that point it just uses \(\epsilon\). 


## What about the Matrix

What remains is to clear up is what our \(*\) operation matrixes does now. What we saw in the post from last week is that the algorithm minimized or maximized some property we calculated. This time it's a little different. Operations like \(max\), \(min\) and \(||\) "discard" one of their operatands but the \(+\) operation from regular expressions doesn't it creates an alternative. So for any node we can use as an intermediate what we get is "Either use the path we already know **OR** the path we can construct by using the intermediate node we are currently testing." Now there might not be a path leading from our start to our target node using the specific intermediate node, then we get back a \(\emptyset\) as an alternative path, which is the only value that is acutally discarded by \(+\). So what we get in the end is a regular expression specifying **all** pathes we can take to get from a start to a target node.

Oh by the way with this we implemented another well known algorithm with the same 7 lines of code I presented in the last blog post. It's called the McNaughton-Yamada algorithm or the Kleene-Construction and is taught to probably every computer science major on the planet. (And forgotten about 5 minutes later ;-) )

## Wrapping up

In the last blog post we started to put our \(*\)-semiring knowledge to use on graphs and found that the same 7 lines of code got us various properties, depending on which semiring we plugged in. This blog post showed how we can describe path between nodes and how we can get all path between two given nodes. Again with the very same 7 lines of code. What remains is to put both of those together to get all path exibiting a given property and that is what we'll do in the next installment. 

