USPTO Project Report
================

**Submitted to:** Professor Roman Galperin

**By:** Keel Scruton (260433121), Yulin Hong (260898713), Carlos Fabbri
Garcia (261018821), Justine Nadeau-Routhier (260483869)

#### Introduction

The United States Patent and Trademark Office (“USPTO”) is the federal
agency for granting U.S. patents and registering trademarks. The USPTO’s
mandate is to “promote the Progress of Science and useful Arts, by
securing for limited Times to Authors and Inventors the exclusive Right
to their respective Writings and Discoveries.”, as per the U.S.
Constitution (Article I, Section 8, Clause 3) \[1\]. Hence, the USPTO
promotes the nation’s technological progress and achievement through the
protection of new ideas and investments in innovation and creativity. To
achieve that, the USPTO employs more than 10,000 patent examiners. The
USPTO faces challenges arising from X and Y, which makes X within Y
difficult. Since there are imbalances in the timing of the USPTO’s
decisions on patent applications, it is becoming a sensitive issue for
inventors. To keep up with pressure from policymakers and regulators,
the USPTO is considering looking more deeply into the patterns of
examiners’ work, including differences in network position and
application processing time based on gender, race and workgroup
categorizations. The central research questions were to understand the
organizational and social factors associated with the length of patent
application prosecution, as well as the role of network structure, race
and ethnicity in this process. The goal of this report is to present and
discuss the main findings of the analysis of the USPTO’s data and to
give objective recommendations with respect to the challenges
experienced by the USPTO, notably on improving the patent application
process and reducing variations in the examination time for patent
applications. This report includes a detailed description of the
methodology used to conduct the analysis, a demonstration of the
analysis and results, a discussion of the results, and conclusions and
recommendations drawn from them.

#### Methodology

By following a series of steps beginning with selecting the data and the
characteristic variables, creating advice networks, carrying out
calculations of centrality score and application processing time, our
consulting team was able to develop a linear regression model and to
provide recommendations to the USPTO for improving the patent
application process. The linear regression model was developed to
estimate the relationship between network centrality and the application
processing time, while also taking examiner demographic attributes into
account. These steps are further elaborated in the following sections.

Selection of data

The two workgroups of examiners selected come from the same technology
center to take baseline similarity in terms of the type of work being
done into account. Group 161 is focusing on organic compounds patents,
whereas 162 is focusing on organic chemistry patents. Because of this,
we expect that there will be advice seeking between both groups on cross
departmental patent issues. Due to the similar nature of the work, any
differences in demographic that appear in the workgroups could be
potentially used as explanation for why one group may be able to process
applications faster than another.

Selection of characteristic variables

The graphs demonstrate that group 161 has more male than female
examiners, while the opposite is observed for group 162, in which the
gender diversity is greater and with the greatest difference from the
average demographic of the entire USPTO examiners. Continuing the
analysis of groups 161 and 162, this time observing race demographics,
it can be observed that both groups are very similar from a race
perspective and do not differ much from the average of the USPTO. White
and Asian examiners are the most common group, while black and Hispanic
examiners make up a smaller minority. These results can be observed in
figure 2.

![Figure 1: Gender Ratios of Selected
Groups](https://raw.githubusercontent.com/ScrutonK/2022-ona-assignments/main/Screen%20Shot%202022-06-05%20at%2010.34.53%20AM.png)
**Figure 1: Gender Ratios of Selected Groups**

![Figure 2: Race Ratios of Selected
Groups](https://raw.githubusercontent.com/ScrutonK/2022-ona-assignments/main/Screen%20Shot%202022-06-05%20at%2010.36.18%20AM.png)
**Figure 2: Race Ratios of Selected Groups**

Creation of advice networks

In order to understand the interconnected advice relationships between
examiners we have created graphs that allow us to investigate the
network structure taking into account both the direction of advice, as
shows by the arrow’s direction in the graph, and degree centrality which
has been represented by the size of the node s. From the figure we can
see that those with larger nodes are mostly those examiners who are
asking a lot of questions. That is the arrows are facing away from large
nodes towards others. As well the examiners that the arrows are pointing
towards tend to be smaller, i.e. they do not tend to have many
connections, and there tend to be many of them for each advice seeker.
From this we can conclude that examiners are seeking highly specialized
information that requires visiting different experts for each of their
applications. As well we can see that there is a fair mix of advice
seeking within each group with questions also going back and forth
between the two groups. This is because the topics covered by the two
working groups is very similar, both groups belonging to the same
technology center. We would expect some patents to potentially overlap
the two topics and require expert opinion from both. Observing
individual nodes we can see that there are examples of examiners from
one group seeking advice from an examiner in their own group, and that
examiner in turn seeking advice from the other group. This could be
representing instances of informational brokerage with certain examiners
acting as the broker between the two groups.

![Figure 3: Directed Advice Network Graph with Node Size Showing Degree
Centrality](https://raw.githubusercontent.com/ScrutonK/2022-ona-assignments/main/Screen%20Shot%202022-06-05%20at%2010.38.32%20AM.png)
**Figure 3: Directed Advice Network Graph with Node Size Showing Degree
Centrality**

When re-graphing the network taking into consideration betweenness
centrality represented by the node size we see that the number of larger
nodes decreases. As well we can see that the examiners who tend to ask
for a lot of advice have very low betweenness centrality scores. This is
because those with high betweenness centrality are acting as
informational brokers who promote informational flow throughout the
entire group. The issue that we observe here is that there is a very
small number of examiners who are acting as brokers and helping the flow
of information for both each group and the combined network of both
workgroups. As well we now can see that those examiners identified in
the previous slide as having a high degree centrality and lots of
connections, but who only seek advice do not contribute to efficient
informational flow.  
![Figure 4: Directed Advice Network Graph with Node Size Showing
Betweenness
Centrality](https://raw.githubusercontent.com/ScrutonK/2022-ona-assignments/main/Screen%20Shot%202022-06-05%20at%2012.34.11%20PM.png)
**Figure 4: Directed Advice Network Graph with Node Size Showing
Betweenness Centrality**

**Development of a Linear Regression Model** To study the organizational
and social factors associated with the length of the patent application
processes we fit a linear regression including sociodemographic
variables and network derived variables. Once the model was trained, we
were able to print a report of the coefficients to do a careful
evaluation of the relationship with each factor and the application
processing time, attempting to offer a reasonable interpretation for
each of the significant variables detected. Next, we will proceed to
discuss in detail the steps that lead up to the creation of the model.

First, we maintained the same filters that we applied for the network
analysis part of the work, meaning that we focused on analyzing the
characteristics of workgroups 161 and 162 for reasons that have been
explained above. When filtering the applications for these 2 workgroups,
we get 204,832 distinct applications, and a network composed of 283
different connected vertices by 1116 edges.

For each row in the application dataframe, we calculate the application
processing time, which is the difference between the date of initial
request and that day of acceptance or refusal of the patent. We filter
our data to keep only the rows that have information for one of these
two dates, and we create the new column for application processing time.

We then use our network graph object to call the igraph methods that
efficiently compute the centrality scores we intend to measure. In this
case, as has been show in previous sections, we will explore 4
characteristics from each node: degree centrality, betweenness
centrality, eigenvector centrality, and closeness centrality. Because
closeness centrality turned out to hold null values for most of the
nodes analyzed, we had to discard it from our variable selection for the
model.

Afterwards, we joined the information we had gathered from each of the
283 nodes (each one representing a different examiner) back to the
applications table. This left us with a final dataset size of 62,787
rows of application information to fit in the linear model. The designed
model was a multivariate linear regression with the following formula:

![Figure
5:](https://raw.githubusercontent.com/ScrutonK/2022-ona-assignments/main/Screen%20Shot%202022-06-05%20at%2012.39.36%20PM.png)
**Figure 5: Linear Regression Model to Estimate the Relationship Between
Variables and Application Processing Time**

In total, there were 7 variables that we tested for significance in the
model, as show in the table below:

![table
1:](https://raw.githubusercontent.com/ScrutonK/2022-ona-assignments/main/Screen%20Shot%202022-06-05%20at%2012.40.33%20PM.png)

**Table 1: Independent Variables Tested for Significance on the Model**

The results obtained from fitting the linear model are reported in
section 4, Results. Some additional graphs were plotted to verify the
relationship between independent and target variables and solidify the
interpretations derived from the coefficients’ magnitude and value.

#### Results

Moving on to the modeling section of the project, we begin by presenting
the target variable. The distribution of the target variable,
application processing time, was plotted as a histogram to understand
the trends of said metric.

<img src="https://raw.githubusercontent.com/ScrutonK/2022-ona-assignments/main/Screen%20Shot%202022-06-05%20at%2012.42.24%20PM.png" width="350" height="350" />

**Figure 6: Histogram for Application Processing Time (in days)**

The distribution of application processing times for the files belonging
to workgroups 161 and 162 is shown in the plot to the left. The
distribution peaks at 1,000 days of processing time and afterwards
becomes skewed to the right, reaching some outliers of 4,000 or more
days of processing time

We begin our reporting of model results by warning that the R-squared in
our model landed in the range considered a fairly weak model. This means
that the implication from our model could be inconsistent with a new
sample or with the statistical universe. Still, the model reported some
significant relationships between variables with a 95% confidence, thus
we will present the total summary of our model.

![](https://raw.githubusercontent.com/ScrutonK/2022-ona-assignments/main/Screen%20Shot%202022-06-05%20at%2012.43.13%20PM.png)

**Figure 7: R Generated Report of the Linear Model Fitted for Workgroups
161 and 162 Combined**

As we can see from the figure above, the R-squared of the trained model
is of 0.02 which is considered very low. From the 8 coefficients
computed (7 variables + the intercept), 7 came out to be significant
when predicting the value of the processing time for a given
application. The coefficients are reported in the column “Estimate” of
the picture, where one can detect that certain variables have a direct
relationship with the application processing times (the coefficient is
positive) and others have an indirect relationship (the coefficient is
negative). In section 5. Discussion, we will contextualize the numbers
reported and discuss the implications it can have for the USPTO
organization.

#### Discussion

Based on the statistical summary, we have discovered some interesting
findings. Before we dive into the findings, we would like to state the
definition of different centrality score measurement method. Degree
centrality assigns an importance score based simply on the number of
links held by each node. Betweenness centrality measures the number of
times a node lies on the shortest path between other nodes. Like degree
centrality, Eigenvector Centrality measures a node’s influence based on
the number of links it has to other nodes in the network. Eigenvector
Centrality then goes a step further by also considering how well
connected a node is, and how many links their connections have, and so
on through the network. We then interpret the linear regression
statistic summary results based on these definitions.

We have found that eigenvector centrality is negatively associated with
application processing time, while degree centrality and betweenness
centrality are positively associated with application processing time.
Degree centrality is easy to interpret. Examiners with a high degree of
centrality are connected to a lot of people in the advisory network.
Thus, this means that they took a lot of time consulting others or
answering others’ questions. This will slow the application process
down. Examiners with high betweenness centrality have a great impact on
the information flow in the advisory system, which is like a marked
brokerage position in the network. They would spend a lot of time
connecting separate groups together and helping the information flow,
thus they do not have enough time to work on their own application.
Lastly, the examiners with high eigenvector centrality scores are
connected to other examiners with high scores. They will be able to
access the information and knowledge that their high-score neighbors
have. This will make the advisory process efficient and fasten the
application process.

As for demographic variables, we have found that on average, male
examiners seem to process the application faster than female examiners.
However, because there are a lot more male examiners than female
examiners in the data set, this finding might be driven by an over
representation of male examiners. The black and Hispanic examiners, on
average, tend to process the application faster than the Asian
examiners. And the interaction between white and Asian examiners is not
statistically significant form our analysis. For work group, the organic
chemistry work group processed the application 177 days faster than
those in the organic compounds work group. The interaction between
tenure days and application processing times is statistically
insignificant.

![Table
2](https://raw.githubusercontent.com/ScrutonK/2022-ona-assignments/main/Screen%20Shot%202022-06-05%20at%2012.52.16%20PM.png)

**Table 2: Implication of the Variables Regression Coefficients for
Assessing their Effects on the Model**

#### Recommendations

Based on the analysis above, we came up with three suggestions for USPTO
on how to make their patent application process more efficient.

First, USPTO should promoting the creation of broker agents in the
network. Through initiatives activities such as buddy programs, the
organization could help alleviate the current situation of having only a
few examiners with high brokerage of information. Overall, this would
help information flow faster among teams and stop high betweenness
centrality from being an issue for certain key examiners.

Secondly, promoting more diverse groups (in terms of race and gender) is
a safe bet for USPTO, since we have noticed that more diverse workgroups
tend to have faster processing times. Also, study has shown that
companies in the top quartile for ethnic and racial diversity in
management were 35% more likely to have financial returns above their
industry mean, and those in the top quartile for gender diversity were
15% more likely to have returns above the industry mean. Thus, promoting
a more diverse workplace should help the USPTO built a more dynamic and
efficient workplace.

Lastly, the organisation should work on undermining social bubbles in
order to diminish polarization of workgroups and sharing examiner’s
knowledge through social contagion such as social cooperation,
structural diversity and spatial structure.

#### References

\[1\] United States Patent and Trademark Office (USPTO). Available at:
<https://www.uspto.gov/about-us> \[Accessed 1 June 2022\].
