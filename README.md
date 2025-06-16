# Data-Mining-Project-Superstore-Marketing-Data-

📊 **Quick-read!**

For my Data Mining for Business course at Drexel (2023), I performed exploratory data analysis (EDA) and developed predictive models to identify high-value customer segments and aid the Marketing team in refining a data-driven targeting strategy for the Superstore’s upcoming premium membership campaign.

This project strengthened my ability to conduct exploratory analysis in complex datasets and utilize supervised learning techniques (Decision Trees) in informing business decision-making.

The analysis highlights key customer segments based on past purchasing behaviors and online engagement. Key traits of high-value customers include **high spending on wine and gold products**, **frequent website visits, recent purchases**, and **active catalog shopping**.

📍 **Stakeholders:** Marketing, Sales, Merchandising, and Operations

📍 **Technical Skills:** R Studio, Exploratory Data Analysis (EDA), Decision Trees (CART & C4.5).




## **Project Description**

Ahead of the holiday **premium membership promotion**, the Superstore sought to refine its marketing strategy by prioritizing customers most likely to engage. Since follow-ups are time-intensive and costly, the marketing team aimed to **identify key customer profiles** that warrant higher engagement efforts.


### **Key Actions Taken**

- **Data Exploration & Cleaning:** Conducted **exploratory data analysis (EDA)**, including *variable distribution plots, correlation matrix analysis*, and *data preprocessing*.
- **Predictive Modeling:** Built Decision Tree models (CART & C4.5) to classify and predict customer responses.
- **Customer Profiling:** Identified behavioral and transactional patterns that correlated with positive responses.

  
### **Model Evaluations**
<img width="315" alt="image" src="https://github.com/user-attachments/assets/89d93e08-634f-4378-b14e-b2ac5a0a964b" />

*Metrics*

— There are slight increases in both accuracy and precision moving from training to test set, indicating that the model is not prone to overfitting and is more precise on the test set. However, the low recall suggests that it is conservative in identifying positives and still misses many true responders

*Next Steps*

- Utilize Cost-matrix training to optimize Precision
- Adjust thresholds to improve the balance between precision and recall according to business priorities
  

### **Business Insights & Outcomes**

The analysis provided a **guide for customer segmentation** based on past purchasing behaviors and online engagement. Key traits of responsive customers included high spending on wine and gold products, frequent website visits, recent purchases, and active catalog shopping. These are customers who are *frequent and loyal to the business, have more spending power, and are more susceptible to deals.* 

Continuous fine-tuning of the model can lead to better predictions in behaviors, informing the team of potential high-value customers.

From these insights, the marketing and sales teams could:

- **Refine targeting strategies** by tailoring offers that focus on customers fitting these behavioral profiles.
- **Prioritize high-value leads** to maximize conversion rates and reduce wasted follow-ups.
- **Improve resource allocation** by directing efforts toward more responsive segments.
