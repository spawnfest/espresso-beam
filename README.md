![My image](https://github.com/Spawnfest2012/espresso-beam/raw/master/snapshots/snapshot.png)


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
In this world, the following things can happen:

*Rabbits can eat carrots.*  
*Wolves can eat rabbits.*  
*Carrots... stay still and wait to be eaten.*  

Moreover,

*Wolves can form **wolves pack** and chase rabbits!*

Our main goal has been to 

 * model the world
 * play with the world params and try to make it lasts as long as possible, i.e.
   try to avoid the immediate extinction of all the species

**Note**: Since we didn't find a graphic character for the wolf, we replaced him with the 
infamous **tac nayn** [1]!

How
---
No surprise, carrots, rabbits and wolves have been implemented by means of finite state
machines. The hard part -- of course -- was to synchronize the processes together, avoiding inconsistent states of the application.

The moving behaviour of carrots and wolves has been implemented in the `kinematics` module,
which exports the following functions:

    wander/2  %% wandering
    seek/2    %% once a rabbit has seen a carrot nearby, try to approach it
    flee/2    %% once a rabbit has seen a wolf nearby, run away!
    pursue/2  %% once a wolf has seen a rabbit nearby, chase it!

At each moving step, both wolves and rabbits lose a bit of their health.
If they are able to eat something, their health level will raise.
If they don't eat for a long time, they will die.

Every time a carrot is eaten, a new carrot is spawned and added to the world, 
keeping the number of carrots constant.

Finally, the **wolf pack** behaviour, has been implemented by the experimental
`pg` module: when a wolf sees a rabbit close to him, it immediately notifies
his fellows calling

    pg:esend(wolves, {chase_that_rabbit, R})

All wandering wolves will immediately chase that rabbit.

The user interface, that is, the html page that you opened before in your browser, 
contains an HTML5 canvas, which at every game step by a javscript controller.

To get the world status, we query the game engine using websockets.
The game engine makes use of cowboy to handle the request, collects the 
state of the world, then gives it back in JSON format.

Who
---
Loris Fichera  <loris.fichera@gmail.com>   
Mirko Bonadei  <mirko.bonadei@gmail.com>   
Paolo D'Incau  <paolo.dincau@gmail.com>    

Why?
----

Here in Italy we have the concept of *zona Cesarini*, which means *in extremis*.   
Well, it was mid June and we were, still, without ideas for the spawnfest.   
Suddendly, a nice thought came up to Mirko's mind... he remembered of a 
curious exercise that *Francesco Cesarini* did when he was a student in Uppsala University [2].
*We can make something similar!*, we thought.  
Well, this has been our *zona Cesarini*, for real. :)

What else?
----------
There are a lot of things in order to make *A tale of blah blah blah* a better software.

But, hey, spawnfest it's just 48 hours and... we have **been working from three 
different places** (one of us has been even **travelling** for work while doing the spawnfest) and,
last but not least, **we never met in person before the spawnfest**. 
Being able to take part to the spawnfest and submit something is our **little victory**. :)

Going onto practical ground, the following things could be implemented/improved:

 * actors must not go outside of the playing field
 * kinematics can be improved: sometimes movements are not too... *intelligent* :)
 * implement spawn of new rabbits/wolves when their health level goes over a certain threshold
 * let the user choose the parametes from the html page
 * refactor env_manager and use ets instead of lists to store the game status
 * names harmonization

Links
----

[1] http://www.youtube.com/watch?v=OM-9Q0ac6Zs   
[2] http://pdincau.wordpress.com/2011/04/25/an-interview-with-francesco-cesarini-francescoc/