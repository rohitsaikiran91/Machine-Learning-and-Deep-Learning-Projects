# Preventive-maintenance-Predicting-machine-failure
## Problem Statement
    To predict the failure of a locomotive engine on the basis of history of operation and maintenence. The predicted result should be 'Component Replacement'(as in component replacement is required for this machine), 'Component Repair'(as in component repair is required) or 'NoIssue'
## Description of the DATASETS
- The following five datasets contains history of Locomotive engines used at a company. There are four models of engines(namely 'model1','model2','model3','model4' ).
- The description of the contents in each of the datasets is provided below
  - Machine Details: 
    - List of all machines
    - Model of each machine
    - Service period
  - Complaint Log
    - Errors logged in each machine with time
    - Five types of Errors
    - Service Log
    - Date, time and type of services done on each machine
    - Only two types of services listed
  - Components replaced
    - History of components replaced(also a type of service)
  - Operating conditions
    - Timestamp of sensor data (Data of four sensors were logged against timestamp. The log is at random intervals.)

## Feature Engineering
The following features were extracted
![Image of FEATURES](https://github.com/rohitsaikiran91/Preventive-maintenance-Predicting-machine-failure/blob/master/FEATURE_ENGINEERING/FEATURES1.JPG)
![Image of FEATURES](https://github.com/rohitsaikiran91/Preventive-maintenance-Predicting-machine-failure/blob/master/FEATURE_ENGINEERING/FEATURES2.JPG)
![Image of FEATURES](https://github.com/rohitsaikiran91/Preventive-maintenance-Predicting-machine-failure/blob/master/FEATURE_ENGINEERING/FEATURES3.JPG)
![Image of FEATURES](https://github.com/rohitsaikiran91/Preventive-maintenance-Predicting-machine-failure/blob/master/FEATURE_ENGINEERING/FEATURES4.JPG)

## Exploratory Data Analysis
![Image of FEATURES](https://github.com/rohitsaikiran91/Preventive-maintenance-Predicting-machine-failure/blob/master/EDA/EDA1.JPG)
![Image of FEATURES](https://github.com/rohitsaikiran91/Preventive-maintenance-Predicting-machine-failure/blob/master/EDA/EDA2.JPG)
![Image of FEATURES](https://github.com/rohitsaikiran91/Preventive-maintenance-Predicting-machine-failure/blob/master/EDA/EDA3.JPG)

## Model Building
![Image of FEATURES](https://github.com/rohitsaikiran91/Preventive-maintenance-Predicting-machine-failure/blob/master/MODELBUILDING/MODEL_ELASTICNET.JPG)

The training dataset was modified to create three more training data sets. In each data set, two out of three output labels were combined, i.e. in the first dataset 'Component Replacement'was considered one output where as other two were combined into an output label 'other'.

![Image of FEATURES](https://github.com/rohitsaikiran91/Preventive-maintenance-Predicting-machine-failure/blob/master/MODELBUILDING/XGBTREE.JPG)

Further, stacking of predictions from ElasticNet, XGBTree with SVMs was done.

![Image of FEATURES](https://github.com/rohitsaikiran91/Preventive-maintenance-Predicting-machine-failure/blob/master/MODELBUILDING/XGBTREE_STACK.JPG)
