# Simplified Enigma Machine in Haskell #

## About this Project ##

This project conforms to an assignment brief provided by a professor at my university. It aims to simulate both the Enigma machine and the Bombe machine - used to break the enigma and the result of the efforts from the code breakers working at Bletchley Park. In the following sections, I will explain the different aspects of these machines and how they work. Each section will have a link to a wikipedia page with more information on their respective topics. 

If you wish to skip to the section on how to run this code and use the functions, click HERE.

## The Enigma Machine ##

In this section, I will briefly outline how the Enigma machine worked in reality, as well as the abstracted versions that I have implemented. For more information, please visit the wikipedia page [HERE](https://en.wikipedia.org/wiki/Enigma_machine).
The Enigma machine was an encryption/decryption device that was built upon rotors, each being a fixed alphabetic substitution cypher. Below is a list of the pre-defined rotors I used for this project, along with their notch positions:
* plain - ABCDEFGHIJKLMNOPQRSTUVWXYZ
* R1 - EKMFLGDQVZNTOWYHXUSPAIBRCJ, 17
* R2 - AJDKSIRUXBLHWTMCQGZNPYFVOE, 5
* R3 - BDFHJLCPRTXVZNYEIWGAKMUSQO, 22
* R4 - ESOVPZJAYQUIRHXLNFTGKDCMWB, 10
* R5 - VZBRGITYUPSDNHLXAWMJQOFECK, 0

With these rotors, if you input 'A' which is position 0 in plain text through R3, you would get 'B'. Each rotor can also be offset by an integer in the range 0 - 25, for example if we had the same input as before but R3 now has an offset of 2, the output would instead be 'F'. The Enigma machine consisted of three rotors: the left rotor `(LR)`, the middle rotor `(MR)`, and the right rotor `(RR)`. Each day, the machine was also initialised with a set of corresponding offsets `(OL, OM, OR)`; to simplify this project I have made the assumption that the initial conditions were reset for each message, though in reality it was much more complicated.

For each character in the message, the `RR`'s offset is increased by 1 (or returned to 0 if it reaches 26). When it reaches its notch position, the `MR` will also have its offset increased, and the same applies to the `LR` when the `MR` reaches its own notch position. My rotors advance when the notch position is reached, so if your `OR` is 3 and the notch position of `RR` is 5, then `OM` will be increased after two characters. The real Enigma machine has a system known as `Double-Stepping` which can make `OM` increase its offset by 2 in a single character under a specific set of conditions; this is something I have not included in my program. It is important to note that the offsets are increased before the character is encoded.

### Basic Enigma ###
In the basic (unsteckered) Enigma, an input character will be passed through the RR, MR, LR in that order. It will then be transmitted through a reflector (a fixed character-swap) before being passed back through the rotors in reverse order (LR, MR, RR). The standard pairings for the reflector were:
`(A Y) (B R) (C U) (D H) (E Q) (F S) (G L) (I P) (J X) (K N) (M O) (T Z) (V W)`

An example, where R1, R2, R3 correspond to LR, MR, RR and our initial offsets are (0, 0, 25). Before encoding, OR will be incremented making the offsets (0, 0, 0):
![Basic Enigma Example](https://github.com/JamesCordrey/Simplified-Enigma-Machine---Haskell/blob/main/READMEimages/FullBasicEnigma.PNG)

### Steckered Enigma ###
This is a more complex version of the basic enigma, where the reflector is replaced by a stecker. A stecker was created by matching pairs of letters through a plugboard, with up to 10 pairings. The letters that were not paired would remain unchanged.

## Bombe Machine ##

In this section, I will briefly outline how the Enigma machine worked in reality, as well as the abstracted versions that I have implemented. For more information, please visit the wikipedia page [HERE](https://en.wikipedia.org/wiki/Bombe).

### Finding the Longest Menu ###

Military messsages were very formulaic, meaning they would often contain the same contents at the same position of the message. For example, a very short message has a good chance of being "nothing to report", while long messages will often identify its sender early in the message and the weather forecast followed by a place towards the end of the message. These common phrases were called `'cribs'` and were the basis of Alan Turing's method for breaking Enigmas. An example of the weather forecase crib:
![Weather Forecast Crib Example](https://github.com/JamesCordrey/Simplified-Enigma-Machine---Haskell/blob/main/READMEimages/ExampleCrib.PNG)

The codebreakers would find something called the `longest menu` by using these cribs. A `menu` is a chain of characters that link together with no overlap between plain text and the encrypted text. In the example above, one of several menus of length 17 is: `[13,14,19,22,4,3,6,5,21,12,7,1,0,8,18,16,9]`.

### Breaking the Enigma ###

The codebreakers at Bletchley Park were aware of how the Enigma machine worked, knew the rotors and reflector and knew that steckers would sometimes be in use. Their task was to figure out the starting offsets and the steckering and rotor configerations each day. To do this, they identified cribs and found the longest menu for these cribs and then they used this information in the Bombe machine which would find configurations that were possible. This machine would exhaust the potential starting configurations by abusing the implications that cribs have on the steckering of the Enigma machine. An example of how the Bombe machine works is below:

![Breaking the Enigma Example](https://github.com/JamesCordrey/Simplified-Enigma-Machine---Haskell/blob/main/READMEimages/BombeMachine.PNG)

Suppose we are starting at position 21 and have assumed that the steckering for 'Y' is for it to remain unchanged, 'Y' is encoded by the Enigma to 'E'. The final cipher is actually 'V', so we need to add [E, V] to the stecker. This would then mean that position 6 must have an input of 'E' for which the Enigma outputs 'N'. However, the final cipher is 'E', so we need to add [E, N] to the stecker which is a contradiction. This would mean we need to try another make a new assumption for the steckering of our staring character 'Y'. If the bombe machine exhausts all possible starting steckers, it will then try a different set of initial offsets and will repeat this until it has covered all configurations. The result of this process may not provide any possible configurations.

## Running and Using the Code ##

All of the code can be found in the `Enigma.hs` file and it is separated into the different sections outlined above by clear comment breaks. Each function has a concise descrription outlining its parameters and purpose directly above itself.








