### Basic Collisions on the NES ###
### (or "How I learned to love banging my head against a brick wall") ###

### Update: June 08, 2026 ###
The code seems to be functioning as intended. I'm working on some refactoring and rewriting this README, so it's more "tutorial friendly" - I should be adding appropriate comments to the code as well. For now, give it a try and see what you think!

(This readme is a work in progress, but the code as it stands should function well enough. If you see any issues with is, drop me a message or email me at shikisha at hotmail dot com!)

As I said in my last lesson, if you're fairly new to programming NES/ASM 6502, you will want to start with my other demo programs/tutorials:
- [https://github.com/thomaslantern/nes-hello-world](https://github.com/thomaslantern/nes-hello-world)
- [https://github.com/thomaslantern/nes-basic-graphics](https://github.com/thomaslantern/nes-basic-graphics)
- [https://github.com/thomaslantern/nes-basic-sound](https://github.com/thomaslantern/nes-basic-sound)
- [https://github.com/thomaslantern/nes-basic-controls](https://github.com/thomaslantern/nes-basic-controls)
- [https://github.com/thomaslantern/nes-birthday-blast)](https://github.com/thomaslantern/nes-birthday-blast)

These tutorials clarify many things that we won't be addressing here, so please check them out if you haven't already. I would recommend going through them in order.

### Where We Left Off ###

In my last tutorial, Birthday Blast, we combined everything we had done thus far - background graphics, sprites, sound, and using the controller - and created a very simple game. It had collision detection, of a sort - if the cake collided with your sprite, you gained points. If the bombs collided with your head, you died. If you ran into the walls in the background, you could pretend you hurt your head, but it basically did what most simple games do and kept you inside of the required boundaries.

The problem with the code in Birthday Blast (and it was _definitely_ the only issue with the code - there were no issues with "falling through" code based on subroutine calls that I didn't explictly mention, oh no...) was that the code for the background essentially made use of "magic numbers", i.e. it didn't _really_ do any collision detection with the background. This tutorial takes the next logical step and shows you one possible way to code for background collision in general. In other words, you could modify my code and place the background tiles in different places, and the code would still work, a feat which was not true in the case of our beloved Birthday Blast.

### Changes to the NMI ###

There are a number of changes to our NMI, if you compare the code to Birthday Blast. I'll address each of them in order:

1) Adding txa, tya, and extra pha commands: Our first major difference is the following:


>	pha
>		php
>			txa
>			pha
>			tya
>			pha

Instead of just using pha and php, we're now using txa and tya, and a adding several more pha commands. What gives? Well, this is the basic way to push all of your flags and registers to the stack, so that you can retrieve them when you're done with the screen update (nmi). Without doing this, you run the risk of having your values in registers x, y, and a being overwritten (similar for the flags). The NMI updates the screen (assuming the screen is on) about sixty times a second (!), so it is inevitable that the NMI will occur at some point while you're in the middle of a function. If you don't back up your values, you're going to lose them, and that could spell disaster for whatever function you were running.

Hilariously enough, when I first wrote this code, I forgot to add this in, and couldn't figure out why my code wasn't working properly. I would say it's a "best practice" to include this at the beginning of your NMI, and to include the following at the end of your NMI (to retrieve your values):

>			pla
>			tay
>			pla
>			tax
>		plp
>	pla
>	rti

Notice how the pla and plp commands are sort of "backward" to how we stored everything on the stack at the beginning? That's because our stack is LIFO, or last-in-first-out.