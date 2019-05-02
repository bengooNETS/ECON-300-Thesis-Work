import pandas as pd
import boto3

##Reading in CSV file!! :)
payments1 = pd.read_csv("C:/Users/goodm/Desktop/UPenn/UPenn Senior Year/ECON 300/Receipt/BonusPaymentPlan.csv")
payments2 = pd.read_csv("C:/Users/goodm/Desktop/UPenn/UPenn Senior Year/ECON 300/Receipt/BonusPaymentPlan2.csv")
assignId = payments1['AssignmentId'].tolist()
assignId += payments2['AssignmentId'].tolist()
len(assignId)

workerId = payments1['WorkerId'].tolist()
workerId += payments2['WorkerId'].tolist()
len(workerId)

paymentList = payments1['payment'].tolist()
paymentList += payments2['payment'].tolist()
len(paymentList)
paymentList = [str(i) for i in paymentList]

reason = "Thank you for participating in our study. As noted at the end of the study, you were randomly selected to have your investment/work decision payed out. This is that payment."

##Setting up mturk client to then push results
client = boto3.client('mturk', region_name = 'us-east-1')
for i in range(len(paymentList)):
    if (i >= 44):
        client.send_bonus(
        WorkerId = workerId[i],
        BonusAmount = paymentList[i],
        AssignmentId = assignId[i],
        Reason = reason,
        UniqueRequestToken = str(i)
        )

