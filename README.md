### Basic Collisions on the NES ###
### (or "How I learned to love banging my head against a brick wall") ###

### Update: June 08, 2026 ###
The code seems to be functioning as intended. I'm working on some refactoring and rewriting this README, so that it's more "tutorial friendly," and I should be adding more appropriate comments to the code. For now, give it a try and see what you think!

(This readme is a work in progress. If you see any issues with this project, drop me a message or email me at shikisha at hotmail dot com!)

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

*1) Adding txa, tya, and extra pha commands:* Our first major difference is the following:

```asm6502
	pha
		php
			txa
			pha
			tya
			pha
```

Instead of just using pha and php, we're now using txa and tya, and a adding several more pha commands. What gives? Well, this is the basic way to push all of your flags and registers to the stack, so that you can retrieve them when you're done with the screen update (NMI). Without doing this, you run the risk of having your a-, x-, and y-registers get overwritten. The same is true for the flags. Assuming the screen is on, the NMI updates the screen about sixty times a second (about fifty times for the PAL NES), so it is inevitable that the NMI will occur at some point while your code is in the middle of a function. If you don't back up your values, you're going to lose them, and that could spell disaster for whatever function was running.

Hilariously enough, when I first wrote this code, I forgot to add this in, and couldn't figure out why my code wasn't working properly. Make it a "best practice" to include the above code at the beginning of your NMI, and to include the following at the end of your NMI (to retrieve your values):

```asm6502
			pla
			tay
			pla
			tax
		plp
	pla
	rti
```

Notice how the pla and plp commands are sort of "backward" to how we stored everything on the stack at the beginning? That's because our stack is LIFO, or last-in-first-out. At the start of our NMI we pushed our accumulator onto the stack (PHA). Then we pushed all of our flags (PHP). After that, we transferred our x-register's value to the accumulator, and pushed that using PHA (there is no "PHX" or anything like that in ASM6502 code). Similarly, we pushed y to a and used PHA again to put Y on the stack. So our stack would look like this:

```asm6502
former Y-register value (top of stack, which we will get first if we "pull" from the stack)
former X-register value (second highest, we get it second)
former processor flags value (etc...)
former accumulator value
```

At the end of our NMI, after doing everything we need to do while the screen is off, we use PLA to grab our old Y-value, then TAY to put it back into Y; we do the same for our old X-value using PLA and TAX. That only leaves PLP to grab our old flags, and PLA to finally grab our accumulator.

*2) No playerpos variable in NMI, and call to $4014 occurs later:* Another big change is that any updates to the player's position is handled _outside_ of the NMI. One major reason for this is that "non-NMI" processing time is greater than NMI time (I've read that the NES is in the NMI/vblank state for only about 8% of the time), so any logic that can be handled outside of updating the screen should probably be done outside of NMI.

In case you're wondering - having too much in my NMI was the _second_ major error in my code that led to many headaches. I've heard that some games put all of their logic inside of their NMI (I believe Konami may have done this), but that's not what I did here, and I found doing it this way was much easier.

*3) No music code:* All of my code in _Birthday Blast_ that involved music is not here. I didn't feel there was any need for music in this particular tutorial, but if you disagree, let that be an intellectual exercise for the reader!

*4) Code involving _nmi_flags_:* Something I felt the need to implement here was a set of flags that determined where you left off in your last game loop. Did you capture the controller inputs? Was the movement logic (outside of NMI) completed before going to NMI? The basic premise here was that, if something didn't complete during the game loop, it was skipped during NMI.

_(My next update will go into more detail about these flags, i.e. why I chose these as flags, how to implement them, what they mean, possibly some deeper philosophical questions... or just assembly related stuff.)_