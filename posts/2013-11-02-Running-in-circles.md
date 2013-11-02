---
title: Running in circles
tags: math, svg, graphs, graph theory, cycles, cycle freeness
---

Even though the title would work for the reason this blog post is out so late, it should have been published monday evening, I still like to do a little math (it's better for my blood pressure anyways).

But on to the blog post. Last week I had a short chat with a colleague who is working on graph theory, not my field of research but I really like it. She was stuck on the proof of a property which seems to be rather obvious:


<div class="problemstatement">
	Any Graph with at least as many Edges as Vertices contains a cycle.
</div>  


<!--more-->

If I understood her correctly it was for an exercise for her students, so if you are here because you googled the problem set **"Please try to solving it on your own!"** Otherwise I hope you learn something. 

Ok now so let's first clear up for everybody what we are talking about. 

<div class="definition">
### (Undirected) Graph

A **graph** \(G\) is a tuple \(G=(V,E)\) with \(V\) being a set the **vertices** or nodes, and \(E\subseteq P(V)\) with \(\forall e\in E \bullet |e|=2\) a set of **edges**. 

</div>

If you are a mathematician or at least have a good ability to read mathematical definitions, this might be enough for you and you can skip the next paragraph.

For anybody else let me try to explain what this means. First we have a set of vertices \(V\). Every vertex is just a "thing" that we can somehow connect with some other "thing" (vertex) by a relation e.g. train stations, people or dots. The second part, the edges \(E\), are precisely these relations, every edge (\(e\in E\)) connects exactly two "things" (vertices) with one another (\(\forall e\in E \bullet |e|=2\)), going on with the examples above, you can thing of an edge as train tracks connecting two train stations, friendships connecting two people or simply a line connecting two dots. In the special case stated above we can't distinguish between an "origin" and a "target" of the relations, so they are symmetrical which only means if \(a\) and \(b\) are train stations and there is a track between them we can't say the track only goes from \(a\) to \(b\) but not the other way around. There is one more technicality we don't allow loops e.g. one vertex \(v\in V\) having an edge going from \(v\) back to \(v\) is forbidden. For the reminder of this exercise this makes no big difference, but makes the formulation above a little easier. 

For obvious reasons we resort to showing graphs as dots and lines between them. So here is an example:

§tikz(--,>=stealth',shorten >=1pt,auto,node distance=2.8cm, semithick)§
  %\tikzstyle{every state}=[circle]

  \node[state]         (A) {$a$};
  \node[state]         (B) [right of=A] {$b$};
  \node[state]         (C) [right of=B] {$c$};
  \node[state]         (D) [right of=C] {$d$};
  
  \path (A) edge (B)
	    (B) edge (C)
		(C) edge (D);

§endtikz§

Here we have the vertices \(V=\{a,b,c,d\}\) and the edges \(E=\{ \{a,b\}, \{b,c\}, \{c,d\} \}\). Note that I could have written \(E=\{ \{b,a\}, \{c,b\}, \{c,d\} \}\) as well, since sets have no inherent order, which is what I said above with no "origin" and "target".

But let's to go on with our property from above. For a mathematical formulation we need one more thing:

<div class="definition">
### Path

For a Graph \(G=(V, E)\) a **path** is sequence of vertices \(<v_0, v_1, v_2, \ldots , v_n>\) such that for every two vertices \(\{v_{i}, v_{i+1}\}\in E\) holds. A path with \(v_0=v_n\) is called a **cycle**.

</div>

Once again a more intuitive way to state that. A path is probably exactly what you would think it is, if you can go from the first vertex in the sequence to the second via an edge then to a third an so on you have a path. In our example from above \(<a, b, c, d>\) is a path since we can go from \(a\) to \(b\) with the edge \(\{a,b\}\) from \(b\) to \(c\) via \(\{b,c\}\) and from \(c\) to \(d\) with \(\{c,d\}\).

A cycle is then, probably not to you surprise, a path that starts and ends at the same vertex. 

So now once again to our problem. A mathematical formulation of the above is:

<div class="problemstatement">
	For any graph \(G=(V,E)\), if \(|E|\geq |V|\) then \(G\) contains a cycle.  
</div>

In the above example it is easy to see what our property states, the graph has four vertices and three edges, so one less edge than vertices and has no cycle. However you can't add any edge between two of the vertices without introducing one. 

Two remarks here: 

1. The property does not state that we can't make a cycle in a graph with less than edges then vertices. It only states that if we have at least as many edges as vertices we are guaranteed to have a cycle.
2. From your math education in school you might remember that any number of example can't be a proof that this holds in general. 

In this case however the property holds and now let's proof that. 

## Proof

First let's give an equivalent formulation: 

<div class="problemstatement">
	For any graph \(G=(V,E)\) that is cycle-free \(|E| < |V|\) holds.  
</div>

I trust in you seeing that this is equivalent formulation to the one above. 

Our proof now proceeds in two steps:

1. We proof that there is at least on vertex \(v\) in any cycle-free graph that has at most one edge connecting it to one other vertex. Or mathematically stated \(\exists v\in V \bullet deg(v)<2\).
2. We proof by induction, that is, if we have a proof that our property holds for a/all smaller graph(s) we can proof it for a graph having one more vertex. 

For the first part.

### Lemma

<div class="problemstatement">
	Every cycle-free Graph \(G\) has (at least) one vertex \(v\) with \(deg(v)<2\).
</div>

#### Proof (by reductio ad absurdum)
We proof this lemma by reductio ad absurdum or contradiction, that is, we say: "There is a cycle-free graph that has no such vertex" and than show that this is a contradiction, which leads to understanding that our initial idea has to be wrong and the opposite has to be true, which is what we wanted to proof in the first place. Here we go:

**Claim:** There is a cycle-free graph \(G=(V,E)\) with \(|V|=n\) where all vertices \(v\) have \(deg(v)\geq 2\).

If we have such a graph, then we construct a path \(<v_0, v_1, \ldots , v_n>\) as follows: We take any vertex \(v_0\), since this vertex has \(deg(v_0)\geq 2\) there is an edge \(\{v_0, v_1\} \in E\). And we can select \(v_1\) as the second vertex, and so on. Since for all vertices \(deg(v_i)\geq 2\) we can always select \(v_{i-1}, v_{i}, v_{i+1}\) such that \(v_{i-1} \not= v_{i+1}\), so the incoming edge is different from the outgoing edge. But now we have a problem \(<v_0, v_1, \ldots , v_n>\) contains \(n+1\) vertices (just count from \(0\) to \(n\)) but \(V\) only contains \(n\) vertices, so two vertices in this path need to be the same, which means, yes you guessed it, there has to be a cycle. So the graph can't be cycle-free and we have our contradiction and by that know that our lemma holds.

### Theorem 

Now to our main theorem:

<div class="problemstatement">
	For any graph \(G=(V,E)\) that is cycle-free \(|E| < |V|\) holds.  
</div>

We will proof this by induction, which goes as follows, we first proof our theorem directly for the base instance, the smallest graph we want our property to hold for. Since we didn't state otherwise this is the graph with one vertex. Following that we construct a proof for a graph with \(n+1\) vertices given that we have a proof for a graph with \(n\) vertices.

So let's see how this goes. We start with the base case.

#### Proof

**Base case:** A graph \(G=(V, E)\) with only one vertex \(|V|=1\) can obviously have no edge without having a cycle. 

This takes care of the base case.

**Induction hypothesis:** For every graph \(G=(V,E)\) with \(|V|=n\), if \(G\) is cycle-free \(|E|<|V|\) holds.

**Induction step:** Let \(G=(V,E)\) be a cycle-free graph with \(|V|=n+1\) vertices. Than, due to our lemma from above, \(V\) contains at least one node \(v\in V\) with \(deg(v)\leq 2\). We construct a new graph \(G^{\prime}=(V^{\prime}, E^{\prime})\), where \(V^{\prime}=V\backslash\{v\}\), so we remove \(v\) from \(V\) and we construct \(E^{\prime}\) by removing all edges connecting \(v\) with any other vertex \(E^{\prime}=E\backslash \{\{u,v\} | u\in V\}\). Obviously \(|V^{\prime}|=|V|-1\), since we removed 1 vertex, and \(| E^{\prime} | \geq |E|-1\), since we removed at most one edge, due to \(deg(v)\leq 1\). As we couldn't have added a cycle by removing one vertex \(G^{\prime}\) is also cycle-free. We can now use the induction hypothesis, because \(G^{\prime}\) only contains \(n\) vertices and is cycle-free. By that we know \(| E^{\prime} | < | V^{\prime} | = n-1 \), therefore \(|E| < n\).

And we are done. The rest is taken care of by the induction principle. Since we have the base case, we can use the induction step to get a solution for two vertices, after we have that we can continue for three, four and so on.

I hope some of you at least tried to suffer through this and have learned a thing or two. And if this was a problem set for one of your math courses, I'm glad I could help, but please try to do your problem sets on your own. You really need the training to sharpen your brain and trust me I know how hard it is to do this in the beginning.
