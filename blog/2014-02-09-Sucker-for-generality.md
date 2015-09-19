---
title: Sucker for generality
tags: haskell, algebra, sets, magma, semigroup, monoid, semiring
---

Lately, for reasons which slip my mind right now, I had to review some old library code I wrote. And as always when you review code you wrote a while ago, you think "How could I have ever thought this was good code?" Usually this is because the code contains more hacks than clean code or because you can't even understand what that piece of code does. In this case I was quite ok with that, but could have hit myself hard for not recognising a general pattern of all distinct cases I programmed back then. This prompted me to go to:
 
 1. Rewrite the code in the more general way, reducing the code size by 30% and increasing the speed on two cases and  
 2. Go into research mode for finding even better patterns of generalisation. 
 
I can't really show you the difference in 1. because I wrote the code for a project I was hired for and it's not really open source but I decieded to make a series of blog post about 2. I hope you like it.

<!--more-->

## Preliminarys

Since I am a theoretical computer scientist and relly like Haskell, probably much of these will take the form of "Applied Abstract Algebra" or "Applied Category Theory" in Haskell. Therefore I decieded to do the following: Every part in this series will be divided into (at least) blog posts, the first explaining the theoretical foundations, like which algebraic/categorical structure or concept we will use, introducing some examples and some code in Haskell to use these. Subsequent blog posts will make use of the implementation and give general algorithms to solve different problems.

As this is the first blog post in the series we will start with theory. In this case Sets, Magmas, Semigroups, Monoids and Semirings.

## Sets 

Actually a ***Set*** in the mathematical sense is not that easy to define, there are several alternatives to do so, because the naive definition "A collection containing everything satisfying a given property" is flawed. I won't go into that (right now). For the moment let us say a *Set* is a collection of things which we can consider as an object of it's own. We dodge the question of how to form a Set and only say someone made one for us and we can work with that.

### Examples

I think everybody knows some examples of Sets, let me give some anyways

* \(\mathbb{N}=\{0,1,2,3, \ldots\}\) The set of natural numbers is a set. 
* \(\mathbb{Z}=\{0, -1, 1, -2, 2, \ldots\}\) The set of intgers is a set.
* [All vertices in a Graph form set, as do all edges](/blog/2013/11/03/Running-in-circles.html)
* \(\ldots\)

### Haskell 
In the code we will present later, we can usually assume that collectin of every member of a type forms a Set, so I don't give code for Sets here. If we really need to use Sets we can use Data.Set from the Standard Haskell Library.

## Magma

A ***Magma*** or a Grouppoid is probably one of the algebraic structures, almost nobody knows. A Magma is a Set \(X\) equipped with on binary operation \(\oplus:X\times X\rightarrow X\) which is closed in that Set, with no further restrictions. This is so simple that everybody who has ever written a programm must have seen a Magma without noticing it and probably has written something which is a Magma themself. Let's go over this with an example:


### Examples 

Take the natural numbers \(X=\mathbb{N}\) and the operation \(\oplus=+\) as the usual addition we all know from school. This forms a Magma because: 

1. We have a Set \(X\) in this case the natural numbers \(\mathbb{N}\).
2. We have a binary operation \(\oplus\), that is a operation that takes two inputs and returns one output, in this case \(+\), and 
3. This operation is closed in the Set, which means, whichever two elements we select as inputs we get a element of the same Set as output. 

Let's do another example: 
We take the natural numbers as our Set \(X=\mathbb{N}\) again and as the operation \(\oplus\) we take the minimum this time \(min(x,y)\). This also forms a Magma.

1. We have a Set \(X\), again the natural numbers \(\mathbb{N}\).
2. We have a binary operation \(min(x,y)\).
3. Independent of our choice of \(x\) and \(y\) \(min(x,y)\) will always return a natural number.

And a third example: 
We take the integers as our Set \(X=\mathbb{Z}\), as binary operation we take \(\oplus=-\) the usual subtraction. This also forms a Magma. 

1. Again we have a Set \(X\) this time the Integers \(\mathbb{Z}\).
2. We have a binary operation \(-\).
3. Independent of our choice of elements from \(\mathbb{Z}\) \(-\) returns an element of the Integers.

### Haskell
In Haskell we can make a type class for Magma by the following snippet and define the examples above as instances.

~~~~ {.haskell}
class Magma a where 
  (<+>) :: a -> a -> a

data AddMagma = AddMagma Integer
data MinMagma = MinMagma Integer
data SubMagma = SubMagma Integer

instance Magma AddMagma where
  AddMagma a (<+>) AddMagma b = AddMagma $ a + b 
  
instance Magma MinMagma where
  MinMagma a (<+>) MinMagma b = MinMagma $ a min b 

instance Magma MinMagma where
  SubMagma a (<+>) SubMagma b = SubMagma $ a - b 
~~~~

## Semigroup

Now a little more than a *Magma* is a ***Semigroup*** it's again a Set \(X\) with a binary operation \(\oplus\) closed in the set. But we now require a little more of the binary operation, it needs to be **associative**, that means whenever we take three (not necessaryly distinct) elements \(a,b,c\) of the Set \(X\) the following must hold
\[(a\oplus b)\oplus c=a\oplus (b\oplus c)\].

### Examples
Let's check our three examples from above if this holds. 

For the Magama \((\mathbb{N}, +)\) we have already checked that we have a Set and a binary operation closed in the set what remains is to check for associativity. I won't formally proof that and hope you see that
\[(a + b) + c = a + (b + c) \] holds for natural numbers. So \((\mathbb{N}, +)\) is also a Semigroup.

For the Magama \((\mathbb{N}, min(x,y))\) we have also already checked that we have a Set and a binary operation closed in the set and have to check for associativity. Again no formal proof but a more detailed look at 
\[min(min(a, b), c) = min (a, min (b + c)) \]. On the left side we first take the smaller number of \(a\) and \(b\) and than compare that with c to find the smaller number among those on the right side we first find the smaller of b and c and than compare that to a to find the smalles of those two. Let's assume the numbers are related in the following way \(a<b<c\) than \(min(a,b) = a\) and therefore \(min( min (a, b), c) = min (a, c) = a\) which is the same as \(min(a, min (b, c))=min(a, b)=a\). You can go through the result with other relations to finally come to the conclusion that \(min(x,y)\) is also associative. Which is to say \((\mathbb{N}, min(x,y))\) is also a Semigroup.

Now for our third example from above, \((\mathbb{Z},-)\). This doesn't form a Semigroup which we can see by taking \(a=7,b=6,c=5\) and put it in \((7-6)-5=1-5=-4 \not= 7-(6-5)=7-1=-6\) so obviously this is not associative. As a result \(\mathbb{Z},-)\) is a Magma, but not a Semigroup.

### Haskell 
In Haskell we can't state the property of associativity, so we can't really give a formulation for that. In a case where we need it we would have to proof it by hand. But what we can state is the property that every Semigroup is already a Magma. This is easy to see from what we required of a Magma and a Semigroup above. 

~~~~ {.haskell}
class Magma a => Semigroup a where 
~~~~

## Monoid

We come to a third structure the ***Monoid***, which is, again, a little more than a Semigroup in this case we don't add further restrictions to the binary operation but, require to have one distinct element from the Set which is called a neutral element \(e\) or \(\mathbf{1}\) (which can be quite confusing, see below). The neutral element has the property that it does nothing if the operation is applied to it an any other element. That is to say a Monoid is a structure \((X, \oplus, e)\) where \((X, \oplus)\) is a Semigroup and for every \(x\in X\) 
\[ e \oplus x = x \oplus e = x \] holds.

### Examples

Let's see if we can find such an element for our remaining two examples of semigroups. 

We already established that \((\mathbb{N},+)\) is a semigroup, so now we need to find an element which doesn't change anything if it is added to any other number. It shouldn't be too hard to see that \(0\) satisfies exactly that. If you take any natural number and add \(0\) to it you get exactly the same number back. So we have found our element \(e=0\) of \(\mathbb{N}\) and have that \((\mathbb{N},+,0)\) forms a Monoid. 

As I already said above often times the neutral element is called \(\mathbf{1}\), this is because most of the time natural numbers with multiplication so \((\mathbb{N}, \cdot, 1)\) is used as an example for a Monoid. In this case \(e=\mathbf{1}=1\) where it is not that irritating. But this throws you off as soon as you take any other combination like \((\mathbb{N},+,0)\) in which case \(e=\mathbf{1}=0\).

Now for our second example \((\mathbb{N}, min(x,y))\) again we already established this to be a semigroup, but now we are stuck we can't find any element which won't interfere with our \(min(x,y)\) operation, because if we select any element of \(\mathbb{N}\) to be our neutral element \(e\) we can find one element \(x\) which is larger then \(e\), in which case applying \(min\) won't yield \(x\) as it would have to, but \(e\). So \(\mathbb{N}, min(x,y))\) can't be made into a Monoid. 

Actually we often want something with \(min\) to be a Monoid. For that we use a little trick, we add an element that by definition satisfies the property of the neutral element. We call it \(\infty\) and than say \((\mathbb{N}\cup \{\infty\}, min(x,y), \infty)\) is a Monoid. Before you ask: **Yes that is cheating** ;-).

### Haskell 

Like before we can't state the property that one or e is a neutral element in Haskell, we will have to proof that by hand, but we can at least give the structure. 


~~~~ {.haskell}
class Semigroup a => Monoid a where
  one :: a 
  
instance Monoid AddMagma where
  one = 0

~~~~

Disclaimer: We won't use this code as there is already a Monoid class in the standard Haskell library in Data.Monoid.

## Semiring

So now for the last structure I need to introduce for the next blog post, the ***Semiring*** this time we don't only add another restriction but we combine things we have found above. A Semiring is structure, consisting of set \(X\) with two binary operations \(\oplus, \otimes\) and two neutral elements \(\mathbf{0}, \mathbf{1}\) such that \(X, \oplus, \mathbf{0}\) and \(X, \otimes, \mathbf{1}\) are Monoids and the following hold: 
\[a \oplus b = b \oplus a\]
\[a \otimes \mathbf{0}=\mathbf{0}\otimes a=\mathbf{0}\]
\[a \otimes (b \oplus c) = (a\otimes b) \oplus (a\otimes c)\]
\[(a \oplus b) \otimes c = (a\otimes c) \oplus (b \otimes c)\]

Ok, I see that looks complicated at first let's try to see what it means. We already know what these Monoid thingys are, now we just have two of them that work on the same Set. That should not be to complicated. But what about these properties. You actually have encountered one example of a Semiring in school, withouth explicitly stated as such. The Semiring \((\mathbb{N}, +, \cdot, 0, 1\)\). 

Where \((\mathbb{N}, +, 0)\) is a Monoid, we already know that, I think you can check that \(\mathbb{N}, \cdot, 1\) is also a Monoid on your own. Now for the remaining properties: 

1. \(a \oplus b=b\oplus a\) this simply states that I can switch the operands of the first operation which is called *commutativity*, you can check that this is the case with \(+\) \(a + b\) is always the same as \(b + a\) 
2. \(a \otimes \mathbf{0}=\mathbf{0}\otimes a=\mathbf{0}\) in this case \(\otimes=\cdot\) and \(\mathbf{0}\) is simply \(0\), so this property holds too, just check \(a \cdot 0 = 0 \cdot a = 0\)
3. \(a \otimes (b \oplus c) = (a\otimes b) \oplus (a\otimes c)\) and \((a \oplus b) \otimes c = (a\otimes c) \oplus (b \otimes c)\) you can check these with basic arithmatic, if you add two numbers and multiply them with a third number it is the same as if you first multiply the individual numbers both with third number and add the results of these multiplications.

But why are these two properties you might ask. This is simple, up to this point I never required that \(a \otimes b\) is the same as \(b \otimes a\), so I can't simply switch the operands around. This allows me to use more operations for the \(\otimes\) operation above. Just as an example of an operation where I can't switch around the operands think of subtraction \(7-5=2\) is obviously not the same as \(5-7=-2\).

So what we just did is give a generalisation of this \((\mathbb{N}, +, \cdot, 0, 1\)\) thingy and allow to use other operations than \(+\) and \(\cdot\) and other Sets than \(\mathbb{N}\). Let's for example start by the following example

### Example

\((\mathbb{N}\cup \{\infty\}, min(x,y), +, \infty, 0)\) now this seems completly ridicoules, but will be very helpfull in the next installment of this series. Ok now let's have a closer look. As already established above \((\mathbb{N}\cup \{\infty\}, min(x,y), \infty)\) is a Monoid, as is \((\mathbb{N}\cup\{\infty\}, +, 0\)\) (you just have to say adding \(\infty\) to anything is \(\infty\), as you might have expected). Now we need to check the other properties 

1. \(a \oplus b=b\oplus a\) check above again \(\oplus=min(x,y)\), so we need to check that \(min(x,y)=min(y,x)\) but that is easy to see, I hope. 
2. \(a \otimes \mathbf{0}= \mathbf{0}\otimes a=\mathbf{0}\), again attention \(\otimes=+\) and \(\mathbf{0}=\infty\) so we have to check \(a+\infty=\infty+a=\infty\), I think this you will agree this is correct, especially since I stated this to be the case above.
3. \(a \otimes (b \oplus c) = (a\otimes b) \oplus (a\otimes c)\) and \((a \oplus b) \otimes c = (a\otimes c) \oplus (b \otimes c)\) this is to say \(a + min(b,c) = min(a+b, a+c)\) and \(min(a,b)+c = min(a+c, b+c)\) I think we agree on that too. So \((\mathbb{N}\cup \{\infty\}, min(x,y), +, \infty, 0)\) also forms a Semring. Actually Semirings using \(min(x,y)\) and \(+\) as operations have a name they are called Tropical Semirings and as I already said we will put this one to good use in the next installment.

### Haskell

Again we can't state the properties but we can write out what we basically need

~~~~ {.haskell}
class Semiring a where
  zero :: a
  one :: a
  (<+>) :: a -> a -> a
  (<.>) :: a -> a -> a

data SchoolArithmatic = SchoolArithmatic Integer
data Tropical = Tropical Integer | Ininity

instance Semiring SchoolArithmatic where
  zero = 0
  one = 1
  (SchoolArithmatic a) <+> (SchoolArithmatic b) = SchoolArithmatic $ a + b
  (SchoolArithmatic a) <.> (SchoolArithmatic b) = SchoolArithmatic $ a * b

instance Semiring Tropical where
  zero = Infinity
  one = 0
  Infinity <+> x = x
  x <+> Infinity = x
  (Tropical a) <+> (Tropical b) = Tropical $ min a b
  Infinity <.> _ = Infinity
  _ <.> Infinity = Infinity
  (Tropical a) <+> (Tropical b) = Tropical $ a + b

~~~~


## Wrapping up 

So now I hope I haven't confused you to much, if I have, try reading it again. If it is still unclear and you want to understand it or if you think I might have made a mistake, which is very well in the realm of possible, the comment section is your friend. If necessary I'll update this post accordingly. Next time (not necessaryly next blog post) I'll try to put the above to good use, especially the Semiring stuff.



## Update: *-semiring

I have to make small addition to this post, I forgot to explain one algebraic structure the *-semiring. It is actually a just a small addition to a Semring. We add another operation which we call asteration or star (\(*\)) this operation has to satisfy the following property for any \(x\in X\)
\[x^* = \mathbf{1}\oplus (x\otimes x^*) = \mathbf{1}\oplus (x^*\otimes x)\]
It is whats called a fixpoint operator. Depending on the setting it might e.g. work as repetition or simple be instantiated to a specific value. From this star operation we can derive another which is called plus. Plus simply omits the base case and can be defined by:
\[x^+=x\otimes x*\]

One detail we will need for the next blog post is that these operators can be defined in terms of each other. We defined \(+\) in terms of \(*\). The reverse direction works as follows. 

\[x^*=1\oplus x^+\]

### Example

In our remaining two examples of a semiring we could define the following asteration operations:

In the \((N, +, \cdot, 0, 1)\) semiring \(*\) could be defined of the infinite sum \(x*=1+\sum_{i}^{\infty}x\) this is not very helpful and we wont use it later on. 

In the \((N\cup \{\infty\}, min, +, \infty, 0)\) semiring on the other hand \(x* = 0\) satisfies our property perfectly. Just by the simple fact that \(\oplus=min(x,y)\) and that the minimum of \(0\) and any other number in \(N\cup \{\infty\}\) will always be \(0\). This is actually more useful and find it's way into the next blog post.

### Haskell 

In Haskell we can write a semiring as follows, obviously for any instance we would have to override one of the both to not fall into the trap of an infinite recursion. For the same reason I will omit a definition of the school arithmatic semiring, it simply wont finish computing anyway. However you will find the definition for the Tropical Semiring below. 

~~~~ {.haskell}
class Semiring a => StarSemiring a where
  star :: a -> a
  star x = one <+> (plus x)
  plus :: a -> a
  plus x = x <.> (star x)
  

class Starsemiring Tropical where 
  star _ = one

~~~~
