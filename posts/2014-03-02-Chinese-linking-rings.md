---
title: Chinese Linking Rings
tags: semiring, algebra, regular expression, haskell, graphs
---

In the last two blog post we learned how to use \(*\)-semirings to implement several different algorithms, with just 7 lines of code and the right instantiation for a class. Among others, I showed you how to the length of the shortest paths between two nodes of a graph. In the last blog post I presented a way to represent every path between two nodes using regular expressions. In this blog post it is the time to combine both. We will construct a new \(*\)-semiring and use our, by now, well known algorithm to get every path that exibits a specific property.

<!--more-->

## Combining \(*\)-semirings

First we need to take a look at if and how we can combine \(*\)-semirings. Actually for the if-part, I probably gave that away in the heading. So the answer is yes and it is pretty simple to combine \(*\)-semirings like this given: \((A,\oplus_1,\otimes_1, \mathbf{0}_1, \mathbf{1}_1)\) and \((B,\oplus_2,\otimes_2, \mathbf{0}_2, \mathbf{1}_2)\) we can simply construct \(( A\times B, \oplus, \otimes, (\mathbf{0}_1, \mathbf{0}_2), (\mathbf{1}_1,\mathbf{1}_2))\) where \((a,b) \oplus (c, d) = (a \oplus_1 c, b\oplus_2 d)\) and \((a,b) \otimes (c, d) = (a \otimes_1 c, b\otimes_2 d)\). This is very simple, but completely useless for our case. Combining one \(*\)-semiring for our property with the one for regular expressions in this way, would just yield for example the the shortest distance for a path as one element of the pair and the regular expression for all path between the two nodes in the other element of the pair. 

We need to do something a little more intelligent to reach our goal. What we want is that the selection of the regular expression depends on the outcome of the operation on the property. Should the path be disregarded because it's longer (less reliable, smaller capacity) we don't want to include it in the regular expression describing the path. 

Let's construct that. For simplicity I will do so with only the shortest path even though it works the same way for the other properties in exactly the same way. You just have to switch the operations in the approrpriate places of this explanaition.

1. First let's use the pairs over the set's of the two semirings to work with. In this case \(\mathbb{N}\cup\{\infty\}\times RE\), we use the left side for our property, the right side for the regular expression describing the path. This should not be a suprise we need to store these properties somewhere. 
2. The neutral elements regarding our new operation will simply be the pairs of the neutral elements from the both \(*\)-semirings we are combining, so for the shortest path these are \((\infty, \emptyset)\) and \((0,\epsilon)\). 
3. We need a \(\otimes\) operation for our new \(*\)-semiring. Let's see what this means for our idea of discarding the path if the path is discarded by the property. In the shortest path \(*\)-semiring we drop a path when we determin the minimum, throwing the longer path away. But we are currently looking at the \(\otimes\) operation which is \(+\) for the tropical semiring, this operation doesn't discard anything. This simply results in the \(\otimes\) operation being defined as it is above, we combine the \(\otimes\)-operations of the semiring like this: \((a,x) \otimes (b, y) = (a + b, x \cdot y)\).
4. Now for the interesting case of \(\oplus\). In the tropical semiring, \(\oplus\) is the minimum-operation. This get's rid of the larger of two pathes, so the same has to go for the regular expression. So depending on which path is the smaller one we discard that half of our regular expression resulting in the following definition of \(\oplus\)

\[(a,x)\oplus (b,y)=\left\{
\begin{array}{ll}
(a, x)& \text{ if } min(a, b) = a \text{ and } min(a, b) \not = b\\
(b, y)& \text{ if } min(a, b) = b \text{ and } min(a, b) \not = a\\
(a, x+y)& \text{ otherwise }
\end{array}
\right.\]

Let's review that. If the minimum of \(a\) and \(b\) is \(a\) we simply discard \(b\) and the pathes \(y\) having the weight \(b\). Same goes the other way around if \(b\) is the shorter distance. In the last case both path have the same distance, so we can use either one to go from start to target node. For our regular expression this means we have to combine both pathes with an **Or** and since \(a\) and \(b\) have the same value we can take either as path length for our pair. 

We now have a semiring for the pair case that does what we wanted. What remains is the \(*\)-operation. This is a bit tricky, but actually not that much. Let's first give it: 

\[(a,x)^*=\left\{
\begin{array}{ll}
(0, x^*) & \text{ if } a = 0\\
(0, \epsilon) & \text{ otherwise}
\end{array}
\right.\]

This might be a little counter intuitive at first, so let's do this thinking thing again. If we have a shortest path from one node to somewhere with distance 0, what does that mean? It means there is a loop which doesn't change the distance. So what can we do with such a loop, right we can take it an arbitrary number of times and this is is exactly what the \(*\) in regular expressions means: "Take this path an arbitrary number of times". So we have to test if \(a\) is equal to the \(\mathbf{1}\) of the tropical semiring, which is \(0\) and if it is we need to add a \(*\) to the path leading to this situation. In all other cases, we are not allowed to take this path which is the same as saying, there is only the path of lenght 0 satisfying the property that it doesn't add any length to the path, which is given by the regular expression\(\epsilon\). 

We now have a \(*\)-semiring for calculating **all** shortest pathes between two given nodes of a graph and again without changing our algorithm from the second to last blog post. For the other properties you simply need to switch, the minimum operation in the definition of \(\ oplus\) for the appropritate operation of the property you desire.  

## Haskell

The following Haskell code works with all the \(*\)-semirings from the second to last blog post, so you just plug in <span class="tt">Tropical</span>, <span class="tt">MinMax</span> or <span class="tt">Relibatility</span> for <span class="tt">a</span> and you get the algorithm for the property you like. 

~~~~ {.haskell}
data BestPath a x = BestPath a x

instance Functor (BestPath a) where
  fmap f (BestPath a x) = BestPath a (f x)
  
extract :: BestPath a x -> x
extract (BestPath _ x) = x


instance (Semiring a, Ord a, Semiring x) => Semiring (BestPath a x) where
  zero = BestPath zero zero
  one = BestPath one one
  (BestPath a x) <.> (BestPath b y) = BestPath (a <.> b) (x <.> y)
  (BestPath a x) <+> (BestPath b y) | ((a <+> b == a) && (not (b == a <+> b))) = (BestPath a x) 
                                    | ((a <+> b == b) && (not (a == a <+> b))) = (BestPath b y)
                                    | otherwise = BestPath (a <+> b) (x <+> y)
  
instance (StarSemiring a, Ord a, StarSemiring x) => StarSemiring (BestPath a x) where
  star (BestPath a x) | a == one = BestPath one (star x)
                      | otherwise = BestPath one one
~~~~

## Admission of cheating

Even though you probably wouldn't have notice at first, but I cheated a little in this blog post. I used a property I didn't previously introduce and which makes all of this work. I used the property that there is a natural order on Real and Natural numbers, which is the reason why the minimum and maximum operations on these Sets are defined. This property allows me to use the equality on these Set's so I can define the \(*\)-semiring like above. So what I need to say is: This will not work in the completly general case of having any property on any set with two operations forming a \(*\)-semiring, but only for those defined on a set which give you an equality. Set's not giving you this property, will probably be the topic of a later round of this thinking deeply about algorithms series.

## Wrapping up

After now having defined a new \(*\)-semiring we can finally find all pathes exibiting a specific property. I have an updated file giving you the full definitions from all the last blog posts, just follow [this link](/assets/documents/Semiring.hs). Next up I will probably give this to you in a programming language "real people use" (someone asked for it) and then let's see if we can venture into a whole different area and let our 7 lines of code rest for the moment.
