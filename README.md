A Tale of Wolves, Rabbits and Carrots
=====================================

For the impatient: how to start it all
--------------------------------------

In your terminal, type:

    $ cd engine
    $ make
    $ make start

Then, open the following url in your browser

    file:///path/to/espresso-beam/ui/index.html

Finally, click on the `start simulation` button. Enjoy!


What
----

*A tale of blah blah blah* aims at being a simulation of a *real world* (oh, well...) 
where inhabitants are carrots, rabbits and wolves.
Simply put, the possible dynamics in our world are:

*Rabbits can eat carrots.*
*Wolves can eat rabbits.*
*Carrots... stay still and wait to be eaten.*

Our main goal was to implement the actors in such a way that the world stay as 
balanced as possible and lasts as long as possible, before the extinction of 
the species.

Note: Since we didn't find a graphic character for the wolf, we replaced him with the 
infamous *tac nayn* [1]!

How
---
Carrots, Rabbits and Wolves


We are a Team of young and nice Italian guys. Here in Italy we have the concept of "zona Cesarini" which means "in extremis".
It was the mid June and we were without ideas about  wht to implement during the SpawnFest 2012. So we thought at the curious exercise that Francesco Cesarini did when he was a student in Uppsala University. We got stuck for a moment thinking... Ehy... This is "zona Cesarini" for real. :-)



Obviously everything is a process from the wolves to the carrot. And everything has been created trying to use OTP behaviours the right way, with supervisor trees and groups to implement restart strategies and the wolves pack feature.

The environment is visible from a Front-End which communicates with the Back-End through Web-Sockets taking advantage of the best Cowboy features.

This is an extremely extensible project. We have developed the main features during those 48 hours but there are lots of features which need to be implemented in the future. Both for fun and for an increase of our knowledge.

Now a couple of words about the main actors of the game.

2- Rabbits:
Rabbits are impemented as gen_fsm. Their main goal is to eat as many carrots as they can without being eaten by wolves.
We have thought at gen_fsm because a rabbit can be in different states during the application lifecycle. A rabbit can be wandering because he has no fear of a close wolf and he has not seen any carrots around.
He obviously can eat a carrot to increase its Healt, or he has to flee away from a wolf. Obiouvsly if a rabbit is not able to eat a carrot for some time he can starve.
In the future the rabbits must be able to give advice to their friends about wolves position and for this feature we will use the pg module.

3- Carrots:
Carrots are implemented as gen_fsm even if they are not complex like rabbits or wolves. A carrot after the spawning is stuck in its position waiting for someone which wants to eat it. After a carrot is eaten the Carrots Supervisor re-spawn another carrot to keep food for our beloved rabbits.

4-Wolves:
Wolves are complex gen_fsm such as rabbits. Their main goal is to eat as many rabbits as they can. They can tell to their pack to follow a rabbit and for this feature we have used the pg module.
A wolf can be wandering around if he is not able to smell a rabbit around, or he can pursue a rabbit to satisfy its needs, because as for the rabbits, wolves can starve if they are not able to eat.


Link
----

[1] http://www.youtube.com/watch?v=OM-9Q0ac6Zs