###GENERAL NOTES

>This uses fleaMarket.simulator, which is set up speifically to be used from other namespaces. As such, there are some things to be aware of.

>First and foremost, you must call 'setup' before calling 'run'. The 'setup' function takes optional keyword arguments. Either way, setup will default to certain values--specified in fleaMarket.simulator--and will assoc-in values from the optional ones passed to it--or assoc-in nothing if no arguments are passed to it. Specifically for genetic programming, the keyword argument paired with :generate-DVE_expr is important. This must be passed an expression that expands to a function. This function should take a market and return a 'determine value expression'. This must be a quoted expression that will extrapolate to a flea's determine value function--more on that later. The program will do the extrapolating, you just need to be aware of what types of things can go into a determine value expression, and what it should return.

>The 'run' function will run a simulation. It takes three keyword args, :size, :duration, and :market. The :size represents how many fleas should be in the market. This will remain constant by inserting new fleas when old ones 'die'--when any of their essential goods drop below a certain minimum 'required for life'. The :duration represents how many rounds there should be in the simulation. The :market represents the starting market for the simulation. Normally, this would simply be ommitted, but it might be useful in continuing a simulation for some 'saved' market.

>On the topic of 'saved' markets, once the simulation has ended, the atom `*`final-market`*` will contain the final state of the market. This will be a long and difficult-to-read hash-map.

>So, see the additional comments below to understand what options can be used in 'setup', as well as what special terms can be used for an expression to generate a DVE--the argument to pair with the optional keyword argument :generate-DVE`_`expr passed to 'setup'--as well as special terms to use from within a 'determine value expression'.

---

###HOW TO READ
Some descriptions inside this comment section represent structures abstractly, so as to generalize. This section describes how to read these representations.

>Bolded things represent a data type. These let you know that something of that data type belongs in that spot.

>>**int** = integer  
>>**+int** = positive integer  
>>**-int** = negative integer  
>>**+int`_`even** = even positive integer  
>>**0`<`n`<`1** = number between 0 and 1  
>>**ratio** = a ratio--technically no different from a 'real number', but contextually should represent a comparison of two numbers  
>>**key** = any keyword (i.e. anything)  
>>**val** = any value (i.e. anything)  
>>**text** = something that will be printed  
>>**good** = any keyword naming a good in the current market  
>>**good-a** = same type as **good**, but used with **good-b** to denote two different goods  
>>**good-b** = same type as **good**, but used with **good-a** to denote two different goods  
>>**DVE** = determine value expression  
>>**g-DVE`_`expr** = a generate DVE expression  
>>**opts** = a hash-map of options  

>Things with `<>`s around them are used to modify placeholders, kind of like shorthands to more succinctly clarify what options can be inserted at that location.

>>**`<`etc...`>`** = repetition of prevous term--as well as any of its subsequent options--could be inserted here  
>>**`<`data-type-1** *OR* **data-type2`>`** = either data-type-1 or data-type-2 could be inserted here

---

###DETAILS FOR 'setup'
>###Notes  
>Any or all of the following options could be passed to setup, as well as any other mappings that you want. For example, if you want to be able to reference some setting ':number-of-trial-runs', then you would simply pass that keyword and some value as additional arguments. I.e. "(setup :number-of-trial-runs **+int**)". Then later to access it, you would call "(get @settings :number-of-trial-runs)".

>###Options  
>(setup  

>>:generate-DVE`_`expr **g-DVE`_`expr**  
>> :market  

>>>{:starting  

>>>>{:world-time **int**  
>>>> :name **int**  
>>>> :fleas **flea-map**}  
>>>> :size **+int_even**  
>>>> :duration **+int**}  

>> :flea  

>>>{:holdings  

>>>>{:essential  

>>>>>{**good**  

>>>>>>{:starting **+int**  
>>>>>> :use-rate **-int**  
>>>>>> :death-below **int**}  

>>>>> **`<`etc...`>`**}}
  
>>>> :luxury  

>>>>>{**good**  

>>>>>>{:starting **+int**}  

>>>>> **`<`etc...`>`**}  

>>>> :currency  

>>>>>{**good**  

>>>>>>{:starting **+int**}  

>>>>> **`<`etc...`>`**}}  

>> :transaction  

>>>{:good-max **0`<`n`<`1**}  

>>:output  

>>>{:begin **text**  
>>> :end **text**  
>>> :break **text**}  

>> **key** **`<`val** *OR* **opts`>`**  
>> **`<`etc...`>`**)

---

###WHAT IS A 'DETERMINE VALUE EXPRESSION'?
>###Notes  
>A 'determine value expression' is an expression of a certain structure that the program will later expand into a function that takes two arguments, 'self' and 'good', and returns a number. Because of how the simulator works, this number abstractly represents the relative value of a given good for 'self'--a flea. This relative value, as its name might suggest, is only meaningful in relation to other relative values, specifically other relative values within the same flea. Inside of a 'determine value expression' you can use the following terms, which, for the sake of understanding, are like macros in that each one simplifies to a larger expression...

>###Special terms

>>`*`good`*`

>>>This represents the current good at hand--the good that has been passed to the :determine-value function. The good whose relative value is being presently determined.

>>`*`all-goods`*`

>>>This is a collection of keywords, each representing the name of a good in the market.

>>`*`information`*`

>>>This is the 'information' that has been gathered by the current flea--'self'. It is a hash-map containing all the ratios of good-a for good-b that this flea has ever traded. It has the structure...

>>>>{**good-a**

>>>>>{**good-b** **ratio**
>>>>> **`<`etc...`>`**}

>>>> **`<`etc...`>`**}

>>>Repeated--denoted by `<`etc...`>`--for every combination of two goods.

>>`*`holdings`*`

>>>This is a hash-map of the current holdings of the flea at hand. A holdings map has the structure...

>>>>{**good** **+int**  
>>>> **`<`etc...`>`**}

>>>Repeated--denoted by `<`etc...`>`--for every good in the current market. The integer here represents the amount of that good the flea 'owns'--so to speak.

>>`*`relative-values`*`

>>>This is a hash-map of the current relative-values of the flea at hand. A relative-values-map has the structure...

>>>>{**good** **+int**  
>>>> **`<`etc...`>`**}

>>>Repeated--denoted by `<`etc...`>`--for every good in the current market. The integer here represents the value reterned by an extrapolation of the :determine-value_expr.

>>`*`good-info-fn`*`

>>>This is a function that, given another good, will return the most recent trade ratio of `*`good`*` to that good. I.e. it will call that from the information hash-map.

>>`*`good-holding`*`

>>>This is the amount of `*`good`*` held by the flea.

>>`*`good-rel-val-fn`*`

>>>This is a function that, given another good, will return the ratio of `*`good`*` to that good--in terms of their relative values from the relative values map.

---

###DETAILS FOR ':generate-DVE_expr'
>###Notes  
>When calling, 'setup' you can pass an optional argument to it paired with :generate-DVE_expr. What is paired with this should be an expression that generates a DVE given a market. Read "What is a 'determine value expression'?" above for more details on that. This expression (that generates a DVE) also has some special options it can use, but in order to use them correctly, you must understand their structure. Inside the g-DVE expression, you can use the following terms, which, for the sake of understanding, are like macros in that each simplifies to a larger expression...

>###Special terms

>>`*`all-goods`*`

>>>This is a collection of keywords, each representing the name of a good in the market.

>>`*`world-time`*`

>>>This is the current world time of the market.

>>`*`fleas-map`*`

>>>This is a map of the current fleas, in which [name flea] typifies each [key value] pair. A name is simply an int, unique to each flea. A flea is more complicated. In the context of the 'evolve-wrapper', a flea is a hash-map of structure...

>>>>{:date-of-birth **+int**  
>>>> :holdings

>>>>>{**good** **+int**  
>>>>> **`<`etc...`>`**}

>>>> :determine-value_expr **DVE**}

>>>So `*`world-time`*` minus :date-of-birth would represent the age of a given flea. The :holdings simply contains how much of each good the flea has. The :determine-value`_`expr is a determine value expression and so can be mutated or recombined with other fleas' DVEs.

>>`*`fleas-list`*`

>>>Instead of a map of fleas, this is a list of the fleas. It is equivalent to (vals `*`fleas-map`*`). As such flea names are not present. This would be simpler to work with if the fleas' names do not matter for evolution.

>>`*`all-determine-value-exprs`*`

>>>This is a list of all the :determine-value exprs for all the current fleas. In other words, it is a list of available determine value expressions.