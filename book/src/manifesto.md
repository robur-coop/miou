# Manifesto

Ce petit manifeste permet d'expliciter nos ambitions par rapport à Miou et notre
coopérative. En effet, au delà des questions techniques que peuvent soulever un
scheduler, il y a aussi des réalités sociales à prendre en compte qui argumentent
l'existence de Miou dans un écosystème qui peut se retrouver fragmenter sur ces
questions. Il ne s'agit pas de partager notre avis sur ce qu'on peut retrouver
entant que projet concurrent à Miou ni prêter des propos et des actes dont vous
ne pouvez être que le seul juge, il s'agit de dire ce qu'est Miou à nos yeux.

This small manifesto aims to clarify our ambitions regarding Miou and our
cooperative. Beyond the technical questions raised by a scheduler, there are
also social realities to consider that justify Miou's existence in an ecosystem
that can become fragmented on these issues. It is not about sharing our opinion
on what might be found as a competing project to Miou or attributing statements
and actions of which you alone can be the judge; it is about expressing what
Miou means to us.

## Protocols, services & Miou

For several years, we have been endeavoring to produce services in OCaml in the
form of unikernels, as evidenced by this website. In pursuing this goal, the
implementation of protocols and services in OCaml naturally comes into play
within our cooperative. It is worth noting that we have implemented many
protocols to date and intend to continue in this direction.

What we have learned over these years is the logical and indeed necessary
separation between the protocol and the scheduler. Even if the sole application
domain of implementing a protocol is I/O, we will continue to produce libraries
that do **not** depend on Miou.

In this regard, we encourage our users to follow the same path. As mentioned in
this tutorial, the scheduler is the final frontier between your application and
reality: the aim is to push this question as far back as possible to ultimately
make the best choice on which scheduler to use.

Furthermore, we will not impose the use of Miou in the majority of the software
we develop and maintain. However, we still have a _slight_ preference for it.

## Miou Extension

For some, Miou is very, even too, minimal. It offers the bare minimum for
developing system and network applications, and it can be tedious to reconsider
(and reimplement) basic building blocks in the design of an application that
Miou does not provide.

Once again, we do not shy away from our goal of integrating Miou into our
unikernels, where this choice to restrict the application domain of our library
becomes a necessity. In this regard, Miou will remain small.

However, this choice can be understood (and we hope our users can understand it
this way) as our humility in not seeking to homogenize and/or standardize an
ecosystem in which we believe you can find your place: a place that may involve
extending or even rebuilding Miou. Thus, Miou's goal is not to be widely used.

All this is to inform our future users that extending Miou to facilitate
application development can be done as third-party libraries but would not be
integrated into Miou.

## Collective Work

At several points in our tutorial, we mentioned that we are not necessarily
omniscient about all possible techniques, for example, regarding synchronization
between multiple tasks. In reality, Miou raises many challenges that can be
daunting in terms of our skills and resources (we are only a small cooperative).

But we believe (and we hope to demonstrate publicly on a daily basis) in
collective work and the contribution of our users to evolve Miou in a direction
that can satisfy everyone. This collective work can be long, sometimes thankless
(and we are the first to acknowledge it), but we would be delighted if Miou were
the synthesis of a community rather than just a few people with very (too)
strong opinions.

Moreover, this is already the case. Miou draws heavily from its _"competitors"_
and builds upon the outstanding work of individuals who have pondered the same
questions as us regarding scheduler implementation. It is a task that can be
lengthy (both in assimilation and implementation) but can correspond, at least,
to our cooperative structure. 

## Acknowledgments

Thus, this little manifesto concludes with thanks to all the individuals who,
directly or indirectly, have contributed to the development of Miou as well as
to the [Robur][robur] cooperative, which has enabled the development of such a
library. Hoping that our opinions and vision for the future of Miou resonate
with you, we will be delighted, even if only to assist you in reclaiming the
means of communication.

[robur]: https://robur.coop/
