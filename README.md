# boost_optimizing_engine

## Boost Optimizing Engine

## Project structure

This is an RStudio Quarto project. The results of this analysis are written up and presented in a rendered HTML Quarto presentation file located [here](https://peterbarwis.github.io/hsd/presentation/boost_optimization.html). This presentation is intended to serve as a means for discussing this project with a panel and providing the documentation/write-up required for this take-home assignment.

The source file to run the analysis is located [here](src/dev.R). This source file needs to be executed in full to generate the objects needed to render the Quarto presentation. A top-level directory called `input` needs to be created, and the `boosts_df.csv` file placed in it, to run this analysis. The source file contains additional comments to explain why various decisions were made.

------------------------------------------------------------------------

## Research questions

1.  How can we optimize boosts to get rides claimed earlier?
2.  How can we optimize boosts to reduce the average cost of rides?
3.  How can we both minimize boost claim time and minimize the average cost of rides?

------------------------------------------------------------------------

## Theory and Approach

My theory is that *drivers* have varying degrees of price sensitivity. For some drivers, smaller boost offers are motivation enough to claim new trips. For others, larger boost offers are necessary. It is the human driver that has the tendency to game the boost offering system, waiting for higher boosts before claiming trips. Accordingly, incentives should be targeted at the driver to influence the driver's claim behavior.

To minimize claim time, we need to remove the incentive drivers have to wait to claim rides, and offer boosts earlier, before the unclaimed time induces anxiety. Increasing boost size as ride time nears incentivizes drivers to wait to claim rides. Instead, we can constrain boosts to a one-time, limited-time, only offer. By doing this, we remove the incentive to wait, because waiting will not improve the boost amount for that driver. By imposing a limited-time constraint, we incentivize drivers to act earlier rather than later. To offer boosts earlier, we can predict which trips are likely to go unclaimed, and then offer boosts for those trips before timing becomes problematic.

To minimize boost size while minimizing claim time, we need to know the price sensitivity of each driver. We can approach more price sensitive drivers first, with smaller boost offers, and limit the time they have to claim a new trip. Trips that go unclaimed during boost round 1 move to round 2, where a higher boost is offered to the second most price sensitive drivers. This cycle proceeds until the trip is claimed or all available drivers are exhausted. If all available drivers are exhausted, then the iteration cycle would be restarted with a higher starting boost offer.

The included project code minimizes claim time by offering individual drivers an optimized one-time, limited-time offer. It minimizes boost size by predicting the price sensitivity of each driver, and offering amounts based on that sensitivity.

## 
