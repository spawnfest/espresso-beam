A Tale of Wolves, Rabbits and Carrots

1- Introduction:
We are a Team of young and nice Italian guys. Here in Italy we have the concept of "zona Cesarini" which means "in extremis".
It was the mid June and we were without ideas about  wht to implement during the SpawnFest 2012. So we thought at the curious exercise that Francesco Cesarini did when he was a student in Uppsala University. We got stuck for a moment thinking... Ehy... This is "zona Cesarini" for real. :-)

The project is a simulation of a real world where there are not humans but only carrots, rabbits and wolves. The main goal is to implement all the actors' behavoiurs and all the concurrency that a system like this needs to be close to the reality as much as possible. The system should be well balanced to avoid the extinction of the species.

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
