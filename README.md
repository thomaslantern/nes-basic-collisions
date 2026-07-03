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

The problem with the code in Birthday Blast (and it was _definitely_ the only issue with the code - there were no issues with "falling through" code based on subroutine calls that I didn't explictly mention, oh no...) was that the code for the background essentially made use of "magic numbers", i.e. it didn't _really_ do any collision detection with the background. This tutorial takes the next logical step and shows you one possible way to code for background collision in general. In other words, you could modify my code and place the background tiles in different places, and the code would still work (which wasn't true for Birthday Blast).