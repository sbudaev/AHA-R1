# Reference model R1 #
## Overview of the simulation results ##

### A brief outline of the model ##

The reference model **R1** implementing elementary self-awareness follows the
simple mesopelagic fish system developed by (Giske et al., 2013, 2014). Here
a population of fish lives in a water column together with stochastic food
(zooplankton) and predators. Zooplankton is randomly distributed within a
thin vertical layer. Illumination changes according to the diurnal cycle
and the zooplankton executes diurnal migrations in a pattern opposite to the
light cycle: goes up at night and down the depth during the day.

**This simple system provides physical gradients, environmental dynamics and
significant stochasticity.**

A fish can orient only by vision and therefore can directly interact only
with objects that it can see, the same holds for the predators. Visibility
of an object (visual range) is a complex function of its size, contrast and
illumination level (Aksnes & Utne, 1997)⁠. To find food, the fish must
follow the constantly changing vertical distribution of the food. Whenever
the fish eats a food item, it obtains energy. All movements and simple
being alive have certain energetic costs, so that the fish will quickly
die of starvation if it does not find food or does not eat for any other
reasons. If the fish eats a food item, all surplus energy not offset by the
living costs goes to the growth and build up of the reproductive factor.

The neurobehavioral system of the fish in the model is built according
to the above architecture and has three survival circuits: hunger, fear
and reproduction. It also has a behavioral repertoire including various
movements (including vertical migrations) immobility, escapes, migration and
can eat zooplankton food items that occur within the visual range. Behavior
selection at each time step minimizes the expected emotional arousal by
re-entrant assessment as described above. Overall, this seems to be a
very complex optimization problem, especially because of the flexible,
unpredictable and not fully deterministic link between the instantaneous
environment and the behavioral action. It might even seem non-obvious if
the genetic algorithm can solve it at all. However, inclusion of the same
individual genetic weights for both determining GOS and prediction of GOS
arousal provides a set of constraints greatly reducing the computational
complexity. Furthermore, we use an adaptive genetic algorithm such that initial
stages of the evolution are much less complex, complexity is increasing as
the evolution proceeds. For example, the life cycle is initially very short
and increases with repeated generations.

#### R1 Model characteristics ####

- **This is a very short and limited simulation over 100 generation. It is
  used for simple illustrative purposes only.**

- Food resource is created for new at the start of each generation and no
  food is replenished between generations. This means that if the agents
  eat all food at the start of the generation, no food is left. This makes
  the optimization problem more complex with each new time step within
  a generation.

- Food items perform diel vertical migration in a pattern opposite to the
  diel illumunation pattern.
  ![Fig. 1](http://ahamodel.uib.no/otherinfo/desc_r1/plot-r1-01.svg)

- The agents select all behaviours based on the minimum expected arousal
  principle. That is, all behaviour is based on elementary self-awareness.

- To ease the initial stages of evolution, adaptive genetic algorithm is
  used. Here the number of time steps in the model is set to a single diurnal
  cycle at the generation 1 (168) and increases to 560 at the generation 100.
  The pattern of this increase is shown on the plot below. Mutation rate is
  also changing according to
  [ga_mutat_adaptive](http://ahamodel.uib.no/doxydoc/classthe__population.html#a3dd42184e1f4e3cad9ef2d887c2c0286)
  function.
  ![Fig. 2](http://ahamodel.uib.no/otherinfo/desc_r1/plot-r1-02.svg)

### Basic results ###

The agents evolving over one hundred generations exhibit a steadily increasing
growth. The number of agents that wrew at the end of each generation showed
a steady increasing pattern.  The number of agents that were alive at the
end of each generation reduced initially (up to generation 60), but started
to rise with further generations (after generation 80).

![Fig. 3](http://ahamodel.uib.no/otherinfo/desc_r1/plot-r1-03.svg)

#### Feeding and growth

Due to the increasing optimization complexity caused by the lack of food
replenishment within a generation, body length and the feeding rate of the
evolving agents stopped growing after approximately generation 30.

![Fig. 4](http://ahamodel.uib.no/otherinfo/desc_r1/plot-r1-04.svg)

However, this was caused by efficient optimization--the agents were able
to eat out almost all of the food available in the environment. This is
illustrated by the following plot that shows the percentage of food items
in the environment that were still available (not eaten) at the end of each
generation. This plot also shows the average perceived number of food items
by the agents.

![Fig. 5](http://ahamodel.uib.no/otherinfo/desc_r1/plot-r1-05.svg)

Clearly, the agents were able to eat out nearly all food resource available
to them in the environment. Thus, approximately after generation 40, their
encounter rate with the food items fell to nearly zero.

The following plot shows the pattern of food consumption (feeding rate,
the number of food items eaten at each time step per alive agents) over the
time steps at the last generation (100).

![Fig. 6](http://ahamodel.uib.no/otherinfo/desc_r1/plot-r1-06.svg)

It is clear that the agents eat out most of the food already during the
first half of the life cycle.

#### Predator avoidance

With each new generation, predation success (numbe of agents killed per time
step) reduced. Thus, the agents evolved more efficient predator avoidance
tactics. The predator perception by the agents (number of predators that they
see) also showed a reducing pattern, but there was a significant sigmoidal
fluctuation. This might have several different interpretations.

![Fig. 7](http://ahamodel.uib.no/otherinfo/desc_r1/plot-r1-07.svg)

#### Response to conspecifics

The agents evolved avoidance of conspecifics, presumably to reduce food
competition. Predator and conspecific perception had similar cyclic patterns
that probably reflect changes of visibility and depth.

![Fig. 8](http://ahamodel.uib.no/otherinfo/desc_r1/plot-r1-08.svg)


### General conclusions ###

- **The AHA model results in a realistic adaptive evolution of the agents over
  the generations.**

- **The significant complexity and indeterminancy caused by elementary
  self-awareness of the agents (their ability to assess and predict their
  emotional state) does not preclude evolutionary optimization.**

- **Evolutionary adaptation of the agents in these conditions involve more
  efficient capture of food items, avoidance of predators and avoidance of
  conspecifics (reducing food competition).**

### References ###

- Aksnes, D. L., & Utne, A. C. W. (1997). A revised model of visual range in
  fish. Sarsia, 82(2), 137–147. http://doi.org/10.1080/00364827.1997.10413647

- Giske, J., Eliassen, S., Fiksen, Ø., Jakobsen, P. J., Aksnes, D. L.,
  Jørgensen, C., & Mangel, M. (2013). Effects of the emotion
  system on adaptive behavior. The American Naturalist, 182(6),
  689–703. http://doi.org/10.1086/673533

- Giske, J., Eliassen, S., Fiksen, O., Jakobsen, P. J., Aksnes, D. L.,
  Mangel, M., & Jorgensen, C. (2014). The emotion system promotes diversity
  and evolvability. Proceedings of the Royal Society B: Biological Sciences,
  281, 20141096–20141096. http://doi.org/10.1098/rspb.2014.1096

