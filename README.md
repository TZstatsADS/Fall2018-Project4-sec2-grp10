# Project: OCR (Optical Character Recognition) 

![image](figs/intro.png)

### [Full Project Description](doc/project4_desc.md)

Term: Fall 2018

+ Team # 10
+ Team members
	+ Ouyang, Fangqi  fo2203@columbia.edu
	+ Shi, Yiming  ys3050@columbia.edu
	+ Wu, Sarah Faye suw2102@columbia.edu
	+ Zhong, Chenzhong  cz2486@columbia.edu

+ Project summary: In this project, we created an OCR post-processing procedure to enhance Tesseract OCR output.In the error detection part, we firstly extract 14 features of each words. Then we perform svm to classify error and correct words. In the post correction part, we combine an empirical OCR error model with LDA topic model to give scores to the candidates, and thus select the best candidate for post correction.

＋ Paper: D3+C5
	
**Contribution statement**: 
+ Zhong, Chenzhong: cleaned data, trained the LDA topic model, realized the OCR error model, and combined topic model with OCR error model to give post-corrections
+ Wu, Sarah Faye: participated in the group meeting about paper understanding
+ Yiming Shi: Paticipated in realizing the OCR error model, and combined topic model with OCR error model to give post-corrections, make ppt
+ Fangqi Ouyang: Complete the error detection part, construct features and tune the parameters for SVM, output error for corrections  



Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
