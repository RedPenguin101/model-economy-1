# Model Economy 1

This model economy is based on Conrad Barski's [EconIsle](https://github.com/drcode/EconIsle).

The model has:

* **Agents**: individuals who have 
    * _desires_, some goal, some gauge of 'satisfaction' or 'happiness' - here expressed in number of burgers, a view not dissimilar to my own. 
    * have the ability to _produce_, with differing capabilities, resources, and in effect act to maximize their productivity.
    * _possessions_, though only of inventory, not of capital, and with the exception of money, the current simulation doesn't allow for creation of surplus.
    * The ability to _trade_ on...
* A **Marketplace** where goods can be exchanged, and a price mechanism for adjusting prices in response to supply and demand. It works by sellers placing 'sell' orders (which all have the same price) and buyers meeting them.
* A **Currency**, or medium of exchange, which avoids barter economy

The sequence of events in this model is:

1. Agents decide what to produce during the day
2. They produce resources
3. They decide how much of the produced resource to sell on the market, and how much for
4. They decide on their 'consumption goals'
5. They go to market and buy things to meet those consumption goals
6. They consume

Or more broadly, 3 phases: produce, trade, consume.

