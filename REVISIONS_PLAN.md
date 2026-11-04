# Plan for CJPS Revisions

This is also essentially a draft of the cover letter we have to write anyways, which should explicitly indicate where and how we have made changes to each of the reviewer's points.

*NOTE TO ROHAN: There are some discrepancies between the manuscript I submitted and the PDF as it is on the repo right now. I will identify and change these, so that then we can be working on the same updated paper. Ive also saved the original manuscript(before CJPS formatting etc. ), so feel free to work directly on the `paper/paper.qmd`.*

## Reviewer 1

- I agree that we should resubmit the Appendix as a single Supplimentary Material file, following the guidelines here: <https://www.cambridge.org/core/services/authors/publishing-supplementary-material>. The Supplement may be a PDF, and has no style restrictions (pubilshed as-is), which makes life easier.  

- I will work on making the wording more clear and less stylistic. Maybe the idea of "travelling" is kind of miselading. Also, good idea to manually verify explicitly that the bumps indeed correspond to what I say they do, this is pretty easy in code. Then, I can include a clarification (likely only one sentence) in the main text explaining how I checked (idea: estimate + CI of by how many more times larger the proportion of the link I think is driving the bump occurs in the bump vs. outside it).

- In Figure 4 (distance distribution), we can change the linetype of the distance markers in the figure generation code.

- Suggested running the models for candidates and EDAs separately. Already did this, and the results were (in terms of sign, significance, and eyeballing coefficient magnitudes) the same, this is why we only show the joint model in the main text. See Supplement D for models fit using specific entities.

- Will be specific in the conclusion, rather than just using "holiday season". Although I'm not sure right now what a better term could be.

- Missing indents will be fixed after we address the more substantive revisions. We'll do this while working on the other formatting stuff we need to do anyways.

## Reviewer 2

- Define "district association" and "localized entity" more clearly the first time they are mentioned.

- **Unclear, Benedict figure out**: Kind of unsure what to do about this. We never claim to be studying donor behaviour, but I agree that this is a very tricky line to be treading.

- Clatify that "smaller districts" are physically smaller in size. Good idea to include raw population size, in addition to density (at least to check if it may be driving what we are attributing to density), and to cite <https://www.tandfonline.com/doi/abs/10.1080/07343469.2019.1572673> if we do find an association.

- **Unclear, Benedict ask Rohan**: Good idea to just divide the median income predictor by 1000 to improve the interpreetability of the model's coefficients. Will also do the same thing with Amount. I don't really understand their point about significant digits vs. fixed decimal points though.

- I think the finding does allow us to infer something about the quality of Canadian democracy, simply because a high degree of localization of donations means that, IF donations bias campaigns or legislator behaviour towards their donor base instead of their constituents, then this structural fact about campaign finance DIRECTLY combats that threat to "good representation". This line of thinking is a little dangerous though because most poli scientists would probably argue that donations have absolutely no effect on the relationship between a representative and their constituents. It's also maybe an over-generalization because there are VERY LIKELY certain districts (probably in Toronto, Vancouver, and Calgary) which do in fact receive WAAAY over the national average (12-17%) proportion of donations from WAAY outside the province (i.e. from other metropolitan areas). I feel it's out of the scope of the paper to investigate this, this is just my thought process. 

## Reviewer 3

### Had one main big claim

Limitation: I describe patterns of OOD, but do not theorize nor test what may be driving the donations. Idea: Look at **characteristics of the recipients of OODs** (requires adding another dataset). Then, could answer:

- Do non-white donors rely more on donations from out-of-district? This would provide some (kinda weak) support for existing Canadian political theory by Garnett that minorities are donating for personal representation instead of party represntation.  
- Test explicitly for an affinity effect.
- Conversely, could test whether candidates receive more OOD if they are in more competitive, or poorer districts.  

Some kind of statistical "affinity" model surely exists (need a new model because we would be analyzing links instead of conventional outcomes), and would be very powerful in supporting the Garnett hypothesis by answering a question that looks like this: "the more your district has these socio-demographic characteristics, the more your candidate-bound OODs are motivated by personal affinity vs. partisan strategy". Ideally, one side of the spectrum would be "personal affinity", and the other side would be "competitiveness", and the model would tell us that donors from districts like X do one thing, but donors from districts not like X do the other. This is a direct analysis of donor behaviour, so we naturally run into the issue of the ecological fallacy. This model would also have to assume that there are only two rational motivations for OOD, which is probably sensible, but have to check the literature. Also, this model might not exist, and so we'd have to invent it, which warrants it's own paper.

This is a cool analysis, but would make the paper much longer, that's my main issue. Plus, this only applies to candidate donations. Can't do this with EDAs (could test for affinity, i.e. whether donors donate to EDAs in districts that share they're demographics, but this isn't providing support to a broader political question or hypothesis, which is what reviewer 3 clearly wants). Also, our paper is literally called "CHARACTERIZING", like c'mon, obviously it's descriptive.

### Other things R3 Mentioned
 
- Reviewer 3 asks: "Do donors in big cities really know they're donating to a candidate in another riding when that riding boundary is only several blocks away?". Surely someone who goes to the trouble of donating over $200 knows what district they live in and who their candidate is. When they fill out the donation form they have to choose an EDA or candidate by name. I think this question is silly. A related but more interesting question of "Are rates of OOD in big cities higher than we might expect from spatial diffusion" is answered in Supplement E. Answer is Yes, very.

- Reviewer 3 asks us to speak more to what kinds of districts are receiving these donations. I've already looked into this, and, at least by looking at the distribution of OOD receipt, it's by far most of them (they all receive a little from out of district). I can see why one might expect that donations are being pumped selectively into a few districts, like in the U.S. But since OODs are so localized in Canada, and most districts are sending out AT LEAST like 20% of their donations outside, it's natural to expect that all districts are getting a little OOD. PLUS, the paper is clearly about the sending districts, not about the recipients. Given this framing, I can see R3's main claim being addressed and fitting it (albeit making the paper longer), but IDK about this one.

## Editor

- Wants us to provide more insight into why OODs occur. States that we are not required to collect more data.

- Says that motivation being from U.S is ok (thank god, because it's a good hook), but should spend more of the Discussion on why these results matter for Canada specifically, and not just in relation to the US. Very fair point, and tihs will require work.

- Fixed the canada "there" not "here" thing.

- When possible during revision, cite CJPS articles.

- **Unclear, Benedict ask Rohan**: what is equity in citation??

## AFTER ALL SUBSTANTIVE THINGS ARE ADDRESSED, WORK ON FORMATTING

STYLEGUIDE: <https://www.cambridge.org/core/journals/canadian-journal-of-political-science-revue-canadienne-de-science-politique/information/author-instructions/preparing-your-materials>


- Needs to conform to CJPS style. There are many more requirements to consider. One big thing is the Word formatting, but we're doing PDF output. For now, I think it's fastest to stick with PDF formatting and worry about theirs later.