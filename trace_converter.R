# set directory of trace(csv) file
getwd()
setwd("/home/ysjin/Documents/SSDLatency")
filename <- "new_hdd_trace.csv"
outputname <- "new_hdd_trace_converted.csv"

# import library of GMM
#install.packages("mixtools") # install mixtool library if not exist
library("mixtools")


# get model data stored in models.txt
models <- new.env()

model_file <- file("models.txt", "r")
line <- readLines(model_file, n=1)
while (length(line) != 0)
{
  # parse header of given model
  options <- strsplit(line, " ")[[1]]
  if (!exists(options[1], envir=models) || is.null(models[[options[1]]]))
  {
    models[[options[1]]] <- new.env()
  }
  if (!exists(options[2], envir=models[[options[1]]]) || is.null(models[[options[1]]][[options[2]]]))
  {
    models[[options[1]]][[options[2]]] <- new.env()
  }
  
  # parse lambda, mu, sigma from file
  lines <- readLines(model_file, n=3)
  data <- strsplit(lines, ",")
  lambda <- as.double(data[[1]])
  mu <- as.double(data[[2]])
  sigma <- as.double(data[[3]])
  
  # store into 'models' structure
  models[[options[1]]][[options[2]]][[options[[3]]]] <- list(lambda, mu, sigma)
  
  line <- readLines(model_file, n=1)
}

close(model_file)


# IOPS information: list(read_information, write_information), list is ordered by size(4K ~ 256K)
hdd_iops_writeonly <- list(list(), list(11466, 9721, 7042, 3898, 1961, 984, 493))
hdd_iops_readonly <- list(list(16440, 13133, 8061, 4041, 2021, 1011, 505), list())
hdd_iops_writedominant <- list(list(5294, 4344, 3230, 2097, 1295, 644, 251), list(2267, 1859, 1383, 898, 554, 276, 107))
hdd_iops_readdominant <- list(list(1873, 1553, 1128, 723, 413, 212, 112), list(4372, 3627, 2629, 1686, 959, 494, 260))

# Sandisk IOPS
ssd_iops_writeonly <- list(list(), list(34592, 25294, 13006, 6142, 3113, 1526, 766))
ssd_iops_readonly <- list(list(48013, 34672, 18710, 10669, 6375, 3503, 1863), list())
ssd_iops_writedominant <- list(list(29344, 18612, 9785, 5677, 3036, 1621, 817), list(12569, 7965, 4192, 2432, 1300, 696, 350))
ssd_iops_readdominant <- list(list(11194, 6833, 3773, 1988, 1007, 507, 253), list(26128, 15957, 8797, 4631, 2335, 1176, 587))



# correction value for matching unit of given trace
correction <- 1 # correction value for FIO
#correction <- 10 # correction value for MSR trace



# read given trace
trace <- read.csv(file=filename, header=TRUE, sep=",")


# threshold for 'idle time'
threshold <- 10000000 # 1 second term for 'idle time'

# list of converted timestamp
new_timestamp <- c()
new_timestamp[1] <- trace$timestamp[1]

startPoint <- 1
for (index in 2:length(trace$timestamp))
{
  # check current progress
  if (index %% 10000 == 0)
  {
    print (index)
  }
  
  # check if idle time is between 2 requests
  if (index == length(trace$timestamp) || trace$timestamp[index + 1] - trace$timestamp[index] >= threshold)
  {
    if (startPoint == 1)
    {
      new_timestamp[startPoint] = trace$timestamp[startPoint]
    }
    else
    {
      difference <- trace$timestamp[startPoint] - trace$timestamp[startPoint-1]
      new_timestamp[startPoint] = new_timestamp[startPoint-1] + difference
    }
    
    # calculate the read:write ratio of separated piece of trace
    readCount <- 0
    writeCount <- 0
    
    for (chunkIndex in startPoint:index)
    {
      if (trace$type[chunkIndex] == 0 || trace$type[chunkIndex] == 'Read')
      {
        readCount <- readCount + 1
      }
      else
      {
        writeCount <- writeCount + 1
      }
    }
    
    if (readCount == 0)
    {
      rwratio <- "writeonly"
      iops_hdd <- hdd_iops_writeonly
      iops_ssd <- ssd_iops_writeonly
    }
    else if (writeCount == 0)
    {
      rwratio <- "readonly"
      iops_hdd <- hdd_iops_readonly
      iops_ssd <- ssd_iops_readonly
    }
    else if (readCount >= writeCount)
    {
      rwratio <- "readdominant"
      iops_hdd <- hdd_iops_readdominant
      iops_ssd <- ssd_iops_readdominant
    }
    else
    {
      rwratio <- "writedominant"
      iops_hdd <- hdd_iops_writedominant
      iops_ssd <- ssd_iops_writedominant
    }
    
    # get expected latency by R:W ratio and I/O size, put new value into responsetime column
    for (chunkIndex in startPoint:index)
    {
      # figure out whether given request is read or write.
      if (trace$type[chunkIndex] == 0 || trace$type[chunkIndex] == 'Read')
      {
        model_rw <- models[["read"]]
        iops_hdd_rw <- iops_hdd[[1]]
        iops_ssd_rw <- iops_ssd[[1]]
      }
      else
      {
        model_rw <- models[["write"]]
        iops_hdd_rw <- iops_hdd[[2]]
        iops_ssd_rw <- iops_ssd[[2]]
      }
      
      # set model and iops by size
      if (trace$size[chunkIndex] < 8192)
      {
        model_req <- model_rw[[rwratio]][["4K"]]
        iops_hdd_req <- iops_hdd_rw[[1]]
        iops_ssd_req <- iops_ssd_rw[[1]]
      }
      else if (trace$size[chunkIndex] < 16384)
      {
        model_req <- model_rw[[rwratio]][["8K"]]
        iops_hdd_req <- iops_hdd_rw[[2]]
        iops_ssd_req <- iops_ssd_rw[[2]]
      }
      else if (trace$size[chunkIndex] < 32768)
      {
        model_req <- model_rw[[rwratio]][["16K"]]
        iops_hdd_req <- iops_hdd_rw[[3]]
        iops_ssd_req <- iops_ssd_rw[[3]]
      }
      else if (trace$size[chunkIndex] < 65536) 
      {
        model_req <- model_rw[[rwratio]][["32K"]]
        iops_hdd_req <- iops_hdd_rw[[4]]
        iops_ssd_req <- iops_ssd_rw[[4]]
      }
      else if (trace$size[chunkIndex] < 131072) 
      {
        model_req <- model_rw[[rwratio]][["64K"]]
        iops_hdd_req <- iops_hdd_rw[[5]]
        iops_ssd_req <- iops_ssd_rw[[5]]
      }
      else if (trace$size[chunkIndex] < 262144) 
      {
        model_req <- model_rw[[rwratio]][["128K"]]
        iops_hdd_req <- iops_hdd_rw[[6]]
        iops_ssd_req <- iops_ssd_rw[[6]]
      }
      else
      {
        model_req <- model_rw[[rwratio]][["256K"]]
        iops_hdd_req <- iops_hdd_rw[[7]]
        iops_ssd_req <- iops_ssd_rw[[7]]
      }
      
      # get new response time by model. negative values are not allowed.
      trace$responsetime[chunkIndex] <- round(rnormmix(1, model_req[[1]], model_req[[2]], model_req[[3]]) * correction)
      while (trace$responsetime[chunkIndex] <= 0 || trace$responsetime[chunkIndex] >= 12000)
      {
        trace$responsetime[chunkIndex] <- round(rnormmix(1, model_req[[1]], model_req[[2]], model_req[[3]]) * correction)
      }
      
      # get new timestamp by referencing IOPS value.
      if (chunkIndex != startPoint)
      {
        difference <- trace$timestamp[chunkIndex] - trace$timestamp[chunkIndex-1]
        new_timestamp[chunkIndex] <- new_timestamp[chunkIndex-1] + round(difference * (iops_hdd_req / iops_ssd_req)) 
      }
    }
    
    # set new startpoint
    startPoint <- index
  }
}

# store new timestamp values to trace
trace$timestamp <- new_timestamp

# store into file
write.csv(trace, file=outputname)


converted_trace <- read.csv(file="new_hdd_trace_converted.csv", header=TRUE, sep=",")
X11()
plot(converted_trace$timestamp, converted_trace$responsetime, cex=.4)


# verifying the latency distribution
converted_trace <- read.csv(file="new_hdd_trace_converted.csv", header=TRUE, sep=",")
ssd_trace <- read.csv(file="new_ssd_trace.csv", header=TRUE, sep=",")
ks.test(converted_trace$responsetime, ssd_trace$responsetime)







######################### previous test values #################################

# lambda, mu, sigma values of Sandisk SSD

#### 4K ####
model_4k_writeonly_write <- list(c(0.1952079,0.4685861,0.03189031,0.3038541,0.000461636), c(27.71116,25.26762,35.9348,31.05365,3440.278), c(0.8019155,0.6301096,6.14365,1.185406,110276.8))
model_4k_readonly_read <- list(c(0.9548034,0.004693018,0.000648593,0.03983859,1.641681e-05), c(21.22352,28.58924,45.14498,24.5621,995.4663), c(0.8228536,3.891036,15.98511,0.6002456,3100.263))
model_4k_readdominant_read <- list(c(0.3385066,0.0001890845,0.01154876,0.5924693,0.002255251,0.05503101), c(22.25175,4019.017,26.71019,20.84772,30.70108,24.63235), c(0.5135282,5432.931,0.8742927,0.4007379,5.664838,0.7057676))
model_4k_readdominant_write <- list(c(0.8020893,0.1968562,0.001054469), c(25.55132,27.85759,86.46227), c(0.856321,1.842707,528.6072))
model_4k_writedominant_read <- list(c(0.4963489,0.002822382,0.001376024,0.1464377,0.3526792,0.000335872), c(21.53045,29.03056,62.11518,25.16538,23.2527,4607.468), c(0.6142702,3.282908,33.74813,1.547018,0.6801394,7205.971))
model_4k_writedominant_write <- list(c(0.3874457,9.550894e-05,0.1076095,0.002390678,0.10256,0.3988645,0.0010341), c(25.5307,318.8659,30.25039,34.25395,28.08604,27.23218,49.88098), c(0.7033173,1154.008,1.597929,3.814423,1.133201,0.8885903,1.777236))

#### 8K ####
model_8k_writeonly_write <- list(c(0.1334821,0.08577698,0.0009852493,0.5260431,0.1496094,0.1040416,6.161603e-05), c(37.01803,40.17963,914.6792,41.87758,40.46744,44.17514,8336.9), c(1.105009,1.25013,695.7392,0.8000568,7.052834,1.328954,28023.57))
model_8k_readonly_read <- list(c(0.9574191,0.03061474,0.01116574,0.0008004105), c(27.90644,29.557,33.62174,62.0364), c(0.5595209,1.086267,0.6170215,124.3558))
model_8k_readdominant_read <- list(c(0.009087326,0.885658,0.0483367,0.05654774,0.0003702543), c(43.03577,36.16248,31.29682,28.16032,4411.808), c(4.782557,2.155794,1.858742,0.5362247,7134.977))
model_8k_readdominant_write <- list(c(0.05720588,0.3137686,0.3363041,0.1461358,0.1456855,0.0009000437), c(32.71909,40.4206,40.59081,42.35221,44.61686,154.8837), c(0.7637057,1.40287,3.454703,0.6665118,0.9101053,828.5058))
model_8k_writedominant_read <- list(c(0.04705507,0.0009387647,0.08004301,0.0004916902,0.27078,0.6006849,6.573306e-06), c(42.68259,5762.383,30.86772,142.0555,39.15333,36.82829,79090.91), c(6.023429,5003.211,2.418825,34.72855,1.339082,0.9970222,87231.82))
model_8k_writedominant_write <- list(c(0.1669257,0.02628353,0.01013041,0.4776956,0.318727,0.0002378527), c(41.02755,32.56278,57.13156,43.15047,41.7324,365.7254), c(4.3094,0.7640544,2.146248,1.712108,0.8131336,1559.598))

#### 16K ####
model_16k_writeonly_write <- list(c(0.02260058,0.06297068,0.2837272,0.2796354,0.1247003,0.2245136,0.001852264), c(67.72989,72.57991,77.52561,77.57747,67.3668,78.55021,1444.975), c(0.7785768,0.9195422,0.8255316,2.210124,10.29665,1.117593,9362.52))
model_16k_readonly_read <- list(c(0.0003688026,0.05015152,0.9443036,0.005176071), c(170.2383,48.21592,48.38996,65.55106), c(236.937,3.741194,1.20317,0.7380095))
model_16k_readdominant_read <- list(c(0.07742013,0.1196528,0.1504868,0.09549724,0.0008757617,0.5556302,0.0004370881), c(52.32279,63.72716,67.18938,63.15799,422.1346,66.0957,7035.721), c(5.718056,1.650445,1.472884,8.312935,355.5269,0.6544559,8169.451))
model_16k_readdominant_write <- list(c(0.03516625,0.1023685,0.01072046,0.2882503,0.1259597,0.4370855,0.0004492424), c(61.48343,75.97664,52.26195,79.79889,72.34646,78.63173,465.8202), c(3.362437,1.474813,0.8442883,1.330708,7.554671,0.7416786,1515.429))
model_16k_writedominant_read <- list(c(0.000994923,0.0005825357,0.691788,0.1016268,0.001637464,0.2033608,9.449999e-06), c(1366.226,175.962,66.74698,61.3064,8844.971,68.68697,103096.6), c(937.4174,45.4634,0.7014041,11.43631,5220.729,1.557331,118976.4))
model_16k_writedominant_write <- list(c(0.01592888,3.900006e-05,0.6597848,0.03968044,0.2600638,0.02368146,0.0008216133), c(52.97931,3610.87,78.73901,66.21712,80.17978,83.0607,108.857), c(1.505618,4870.248,0.8136016,3.731468,1.389664,5.353138,30.56246))

#### 32K ####
model_32k_writeonly_write <- list(c(0.04799278,0.3944974,0.02089812,0.01965579,0.4194088,0.09717286,0.0003742479), c(234.9389,112.1962,1572.634,120.6655,110.9717,113.9562,13059.04), c(136.7119,1.251456,334.1971,7.661255,0.9425262,1.934836,38957.84))
model_32k_readonly_read <- list(c(0.04037676,0.9401456,0.002604732,0.01687287), c(98.96199,96.20842,142.2645,73.7684), c(3.121288,0.7273907,177.8856,3.276534))
model_32k_readdominant_read <- list(c(0.04323372,0.05031816,0.01645003,0.1766589,0.7123059,0.001033249), c(97.07032,104.0824,1043.222,98.85357,96.86761,10189.84), c(18.78985,3.915972,688.0883,1.402531,0.7480028,13132.34))
model_32k_readdominant_write <- list(c(0.01840781,0.02752151,0.6250636,0.0005502153,3.428888e-05,0.3283817,4.08188e-05), c(117.5219,99.13587,111.7169,189.0705,473.9318,113.3495,6856.644), c(3.408918,17.17909,0.9030417,18.31186,225.9238,1.42333,2301.225))
model_32k_writedominant_read <- list(c(0.03459238,0.01388054,0.6170454,0.01695034,0.2437165,0.06892814,0.004886663), c(99.03896,2518.672,97.57218,906.9229,99.78023,105.599,14596.57), c(19.47752,1192.285,0.7170882,525.2262,1.428564,4.622689,18115.35))
model_32k_writedominant_write <- list(c(0.02121282,0.4937393,0.01250769,0.1251785,0.0006608271,0.3466275,7.339368e-05), c(118.8995,111.9127,86.74073,114.3696,185.9032,112.7718,6101.831), c(8.718595,0.8974984,3.967628,1.754647,20.0961,1.233117,5864.695))

#### 64K ####
model_64k_writeonly_write <- list(c(0.254976,0.03706657,0.3493176,0.04980807,0.007141552,0.2315476,0.07014274), c(178.46,205.8541,179.9517,183.947,190.4662,319.6498,1671.605), c(1.212312,4.860755,2.158085,2.73151,2.632835,110.0856,6054.226))
model_64k_readonly_read <- list(c(0.7974267,0.002634006,0.1691953,2.855822e-05,0.004324609,0.005593122,0.02079773), c(157.7014,244.687,158.8169,1327.822,137.0456,131.3325,166.0126), c(0.6071477,65.36792,0.9645284,1795.411,3.983976,1.377512,7.693788))
model_64k_readdominant_read <- list(c(0.05084635,0.08610793,0.03356436,0.4977641,0.2770914,0.05285207,0.001773728), c(1444.557,165.1115,177.7336,158.547,160.2268,176.0957,11529.09), c(934.0138,3.35954,36.00585,0.6336286,1.285846,8.320438,12292.22))
model_64k_readdominant_write <- list(c(0.01504868,0.003059069,0.6778364,0.1022544,0.02125147,0.1804601,8.978137e-05), c(186.8023,262.8175,178.7506,180.8779,171.1908,180.8535,3744.252), c(3.478954,9.415382,1.295002,0.9928645,23.78438,2.092806,3604.254))
model_64k_writedominant_read <- list(c(0.02078374,0.4400536,0.1121372,0.2446667,0.09495786,0.07489649,0.01250436), c(192.5933,159.4443,2170.266,161.8278,165.3087,174.5115,13576.08), c(43.28712,0.7567328,1373.863,1.156563,2.968871,7.207578,13389.52))
model_64k_writedominant_write <- list(c(0.0182856,0.02168162,0.6540018,0.02104267,0.284837,0.0001512987), c(196.7165,176.5021,179.2315,186.0929,180.8876,7739.305), c(42.69128,0.6910065,1.211181,4.181912,1.82279,7912.713))

#### 128K ####
model_128k_writeonly_write <- list(c(0.05467404,0.2599621,0.0565674,0.03262322,0.1952477,0.3987054,0.002220128), c(331.179,313.8962,376.9955,398.7521,1479.706,476.4243,13685.93), c(11.18304,3.031287,26.1069,2.747357,660.5104,56.28695,34449.3))
model_128k_readonly_read <- list(c(0.01236261,0.003334902,0.03430075,0.7400015,0.08950273,0.1204771,2.037782e-05), c(252.5479,386.4659,295.346,280.1796,361.2931,281.9714,2962.115), c(5.149864,62.04758,11.48308,0.6254732,8.026478,1.582171,2565.612))
model_128k_readdominant_read <- list(c(0.1130936,0.3041917,0.2467215,0.1278324,0.07813483,0.1262573,0.003768791), c(304.3466,281.3856,283.6083,288.3431,361.9232,1405.763,11406.49), c(12.56321,0.7359882,1.503175,3.40147,11.7691,941.3781,13728.02))
model_128k_readdominant_write <- list(c(0.101545,0.04051743,0.6413764,0.07484121,0.030849,0.02070137,0.09016952), c(320.0292,312.7319,314.676,314.3624,315.9705,340.0481,398.5934), c(4.303604,0.7007707,2.811164,0.7558907,0.7622379,3.727203,283.8231))
model_128k_writedominant_read <- list(c(0.06412603,0.3993873,0.1720564,0.07646886,0.07318442,0.1941643,0.02061276), c(899.4023,283.7922,288.7133,304.1984,362.5982,3155.314,15137.78), c(414.5002,1.738386,3.444488,10.73958,9.803328,1608.567,15204.03))
model_128k_writedominant_write <- list(c(0.02701549,0.5957964,0.1590274,0.1023643,0.02594387,0.0002274614,0.08962511), c(329.7278,314.0275,313.973,319.3289,338.183,8508.391,397.4658), c(38.39847,2.727053,1.246837,4.134541,3.577498,7849.943,8.599299))

#### 256K ####
model_256k_writeonly_write <- list(c(0.1996133,0.3012292,0.1307395,0.01343261,0.1717567,0.1806755,0.002553162), c(581.0838,936.9418,692.8573,4028.793,1397.551,2556.803,23794.74), c(4.732515,116.2674,62.76776,1754.969,319.793,139.3396,48174.94))
model_256k_readonly_read <- list(c(0.02313957,3.634236e-05,0.03918154,0.7208703,0.01309453,0.2006763,0.003001399), c(547.4407,3570.407,531.5438,525.3464,609.3713,526.7154,1016.871), c(39.01494,2558.186,3.721124,0.6487923,1.917283,1.122515,39.89051))
model_256k_readdominant_read <- list(c(0.2683478,0.1451329,0.1373732,0.1866665,0.1391772,0.1157233,0.007578929), c(531.9672,527.9322,594.4286,546.1852,2660.493,1012.427,13021.32), c(2.82567,1.088753,27.3546,9.616324,1024.887,291.0807,15408.8))
model_256k_readdominant_write <- list(c(0.1328165,0.3797126,0.05477502,0.2722715,0.142871,0.01742336,0.0001301047), c(641.0187,581.4359,573.8252,584.2518,587.9762,609.1327,7620.346), c(35.19989,2.583675,1.719908,3.87371,5.401688,4.143353,2550.508))
model_256k_writedominant_read <- list(c(0.1779745,0.1750869,0.1394647,0.07851506,0.07677221,0.2937696,0.05841714), c(529.0256,536.5239,550.5295,609.5254,1049.131,4005.726,14682.85), c(2.086056,4.839356,10.90945,15.11199,330.4035,1973.704,8627.585))
model_256k_writedominant_write <- list(c(0.05843501,0.1550412,0.1212864,0.2730984,0.2115607,0.04034112,0.1402372), c(571.4344,578.2107,582.2661,579.8318,583.6363,602.6186,680.1421), c(1.554875,2.508347,2.990259,2.410997,4.501677,5.922496,564.4508))

models <- new.env()
models[["read"]] <- new.env()
models[["write"]] <- new.env()

models[["read"]][["readonly"]] <- new.env()
models[["read"]][["readdominant"]] <- new.env()
models[["read"]][["writedominant"]] <- new.env()

models[["write"]][["writeonly"]] <- new.env()
models[["write"]][["readdominant"]] <- new.env()
models[["write"]][["writedominant"]] <- new.env()

models[["read"]][["readonly"]][["4K"]] <- model_4k_readonly_read
models[["read"]][["readonly"]][["8K"]] <- model_8k_readonly_read
models[["read"]][["readonly"]][["16K"]] <- model_16k_readonly_read
models[["read"]][["readonly"]][["32K"]] <- model_32k_readonly_read
models[["read"]][["readonly"]][["64K"]] <- model_64k_readonly_read
models[["read"]][["readonly"]][["128K"]] <- model_128k_readonly_read
models[["read"]][["readonly"]][["256K"]] <- model_256k_readonly_read
models[["read"]][["readdominant"]][["4K"]] <- model_4k_readdominant_read
models[["read"]][["readdominant"]][["8K"]] <- model_8k_readdominant_read
models[["read"]][["readdominant"]][["16K"]] <- model_16k_readdominant_read
models[["read"]][["readdominant"]][["32K"]] <- model_32k_readdominant_read
models[["read"]][["readdominant"]][["64K"]] <- model_64k_readdominant_read
models[["read"]][["readdominant"]][["128K"]] <- model_128k_readdominant_read
models[["read"]][["readdominant"]][["256K"]] <- model_256k_readdominant_read
models[["read"]][["writedominant"]][["4K"]] <- model_4k_writedominant_read
models[["read"]][["writedominant"]][["8K"]] <- model_8k_writedominant_read
models[["read"]][["writedominant"]][["16K"]] <- model_16k_writedominant_read
models[["read"]][["writedominant"]][["32K"]] <- model_32k_writedominant_read
models[["read"]][["writedominant"]][["64K"]] <- model_64k_writedominant_read
models[["read"]][["writedominant"]][["128K"]] <- model_128k_writedominant_read
models[["read"]][["writedominant"]][["256K"]] <- model_256k_writedominant_read

models[["write"]][["writeonly"]][["4K"]] <- model_4k_writeonly_write
models[["write"]][["writeonly"]][["8K"]] <- model_8k_writeonly_write
models[["write"]][["writeonly"]][["16K"]] <- model_16k_writeonly_write
models[["write"]][["writeonly"]][["32K"]] <- model_32k_writeonly_write
models[["write"]][["writeonly"]][["64K"]] <- model_64k_writeonly_write
models[["write"]][["writeonly"]][["128K"]] <- model_128k_writeonly_write
models[["write"]][["writeonly"]][["256K"]] <- model_256k_writeonly_write
models[["write"]][["readdominant"]][["4K"]] <- model_4k_readdominant_write
models[["write"]][["readdominant"]][["8K"]] <- model_8k_readdominant_write
models[["write"]][["readdominant"]][["16K"]] <- model_16k_readdominant_write
models[["write"]][["readdominant"]][["32K"]] <- model_32k_readdominant_write
models[["write"]][["readdominant"]][["64K"]] <- model_64k_readdominant_write
models[["write"]][["readdominant"]][["128K"]] <- model_128k_readdominant_write
models[["write"]][["readdominant"]][["256K"]] <- model_256k_readdominant_write
models[["write"]][["writedominant"]][["4K"]] <- model_4k_writedominant_write
models[["write"]][["writedominant"]][["8K"]] <- model_8k_writedominant_write
models[["write"]][["writedominant"]][["16K"]] <- model_16k_writedominant_write
models[["write"]][["writedominant"]][["32K"]] <- model_32k_writedominant_write
models[["write"]][["writedominant"]][["64K"]] <- model_64k_writedominant_write
models[["write"]][["writedominant"]][["128K"]] <- model_128k_writedominant_write
models[["write"]][["writedominant"]][["256K"]] <- model_256k_writedominant_write








# lambda, mu, sigma values of Samsung 850 PRO SSD
#### 4K ####
model_4k_writeonly_write <- list(c(0.08070185, 1.592227e-05, 0.01147907, 0.900152, 0.0002527608, 0.00633238, 0.001066046), c(24.50123, 2228.123, 27.56948, 23.37234, 60.15415, 35.22809, 47.41051), c(0.8081762, 2338.986, 2.24022, 0.5231311, 14.79468, 2.846391, 5.574177))
model_4k_readonly_read <- list(c(0.0001323601, 0.05342305, 0.9393954, 0.007049198), c(90.12898, 22.95699, 21.83814, 31.75635), c(170.1494, 0.9741304, 0.5026765, 5.494253))
model_4k_readdominant_read <- list(c(0.1316779, 0.01137334, 0.003555009, 0.005249682, 0.01342313, 0.8342991, 0.0004218241), c(23.81122, 91.31227, 45.15984, 229.9023, 30.08151, 22.05597, 1131.996), c(1.462802, 12.995, 2.353949, 115.5105, 4.330111, 0.4813624, 790.7317))
model_4k_readdominant_write <- list(c(0.09806781, 0.8856549, 0.01447519, 0.0004084657, 0.001393593), c(26.17273, 23.71715, 36.77082, 193.0081, 162.0217), c(1.408852, 0.5627089, 7.992416, 481.0901, 1.837591))
model_4k_writedominant_read <- list(c(0.0842084, 0.09268277, 0.03903473, 0.5095719, 0.1613993, 0.08906282, 0.02404006), c(71.80103, 88.69889, 170.9308, 101.9292, 110.7848, 126.4582, 1776.079), c(2.527207, 1.076996, 74.65442, 13.8059, 1.117254, 1.111523, 931.7368))
model_4k_writedominant_write <- list(c(0.1828977, 0.1420721, 0.01407553, 0.03160796, 0.3281508, 0.3008678, 0.0003281806), c(23.14099, 26.7985, 56.55163, 38.29308, 39.2404, 47.54638, 414.123), c(0.5402281, 2.138533, 13.2258, 0.5013079, 6.605169, 0.8121741, 815.6316))

#### 8K ####
model_8k_writeonly_write <- list(c(0.9119806, 0.07776732, 0.01011503, 0.00013707), c(30.54483, 32.26353, 42.76359, 390.3249), c(0.5465081, 0.9775125, 5.488247, 921.9182))
model_8k_readonly_read <- list(c(0.9756572, 0.005078505, 0.01926425), c(28.79721, 43.313, 30.75177), c(0.5436556, 41.71671, 1.124379))
model_8k_readdominant_read <- list(c(0.9544875, 0.04551253), c(29.38796, 195.4739), c(0.8081377, 286.5591))
model_8k_readdominant_write <- list(c(0.9843891, 0.01561093), c(31.24796, 63.34371), c(0.8005877, 94.12685))
model_8k_writedominant_read <- list(c(0.1828674, 0.1782379, 0.2110519, 0.1914044, 0.122409, 0.05872519, 0.05530417), c(146.4772, 105.8338, 104.1107, 120.4935, 127.4705, 143.2541, 1754.132), c(55.49054, 1.455434, 0.9029602, 1.340483, 1.647034, 1.366962, 696.9199))
model_8k_writedominant_write <- list(c(0.000174785, 0.4291972, 0.1901522, 0.2898342, 0.00522229, 0.08541923), c(775.9129, 30.33487, 55.90023, 31.82596, 141.8968, 62.55549), c(1142.183, 0.4969099, 0.8671385, 1.048289, 4.313975, 18.43895))

#### 16K ####
model_16k_writeonly_write <- list(c(0.06971336, 0.004128475, 0.0007446874, 0.9174381, 0.002566006, 0.003141809, 0.002267548), c(47.67166, 54.24516, 5272.008, 46.50545, 69.16903, 169.7755, 604.3078), c(1.032883, 4.051908, 2393.232, 0.6651954, 10.65898, 70.95137, 321.7562))
model_16k_readonly_read <- list(c(0.0003744968, 0.004058968, 0.01829762, 0.8221965, 0.100936, 0.05413634), c(196.2622, 68.44124, 56.61232, 43.47524, 44.71214, 47.83903), c(227.8559, 10.50146, 5.179389, 0.5782562, 0.840007, 2.102295))
model_16k_readdominant_read <- list(c(0.871226, 0.04290553, 0.07015486, 0.01571358), c(44.12677, 133.0819, 46.49969, 1047.221), c(0.5971938, 67.40689, 2.116855, 506.118))
model_16k_readdominant_write <- list(c(0.9177596, 0.04954483, 0.001876814, 0.02011215, 0.01070659), c(46.83518, 48.52281, 222.0888, 54.11228, 79.26154), c(0.6348941, 1.276915, 371.8717, 3.959358, 14.49891))
model_16k_writedominant_read <- list(c(0.1408558, 0.0961414, 0.1639913, 0.3054933, 0.05323179, 0.1501466, 0.09013989), c(123.8624, 155.7763, 216.3523, 161.9981, 187.3027, 193.5131, 1931.268), c(2.281932, 1.466093, 79.75295, 2.397599, 1.145515, 2.137522, 601.0063))
model_16k_writedominant_write <- list(c(0.9328866, 0.06711343), c(46.75545, 73.40646), c(1.311166, 82.62709))

#### 32K ####
model_32k_writeonly_write <- list(c(0.05041794, 0.05726668, 0.6573648, 0.01567485, 0.01215052, 0.2050825, 0.002042763), c(76.4801, 93.41426, 79.71279, 548.6372, 172.681, 80.76314, 4979.6), c(0.6072724, 7.469601, 0.6922496, 284.7939, 57.02448, 1.030141, 3837.242))
model_32k_readonly_read <- list(c(0.02771838, 0.2361937, 0.0003565828, 0.7250931, 0.01063824), c(87.94363, 75.22032, 203.2072, 75.05818, 81.23998), c(6.308428, 1.646261, 192.4643, 0.6474815, 1.633465))
model_32k_readdominant_read <- list(c(0.7726663, 0.01881869, 0.0122036, 0.001538812, 0.01807034, 0.1549085, 0.02179373), c(73.15045, 184.8976, 328.0695, 1784.428, 99.80683, 74.52801, 1512.306), c(0.6340977, 41.66134, 109.901, 1313.623, 16.38144, 1.195549, 230.5904))
model_32k_readdominant_write <- list(c(0.1076068, 0.8753729, 0.015174, 0.00184628), c(79.31255, 77.8406, 105.4255, 289.4221), c(1.198511, 0.6692968, 16.66471, 474.9949))
model_32k_writedominant_read <- list(c(0.1386634, 0.1289394, 0.276753, 0.06813147, 0.1424428, 0.09404665, 0.1510232), c(214.8288, 185.0355, 190.7539, 216.4586, 222.2132, 320.3234, 2120.337), c(34.68871, 1.377448, 1.497991, 0.97815, 1.103734, 108.8809, 489.0494))
model_32k_writedominant_write <- list(c(0.8661825, 0.0701477, 0.01144528, 0.05179554, 0.0004289654), c(76.93714, 121.005, 92.15324, 79.08485, 796.9153), c(0.5939358, 6.461479, 7.937497, 1.550428, 1168.785))

#### 128K ####
model_128k_writeonly_write <- list(c(0.1204092, 0.09503427, 0.200651, 0.3637375, 0.0851053, 0.1293682, 0.005694507), c(262.7563, 332.5844, 267.3982, 266.3623, 282.124, 831.1241, 7454.841), c(1.005737, 36.35762, 1.966832, 1.105975, 5.093669, 312.1816, 4386.509))
model_128k_readonly_read <- list(c(0.02887064, 0.06615236, 0.03047858, 0.4803415, 0.003494261, 0.3895043, 0.001158291), c(247.7564, 259.2191, 265.0215, 252.0504, 286.4052, 252.8853, 418.7901), c(0.7307352, 1.72504, 4.340772, 1.340049, 4.275856, 1.773974, 245.8885))
model_128k_readdominant_read <- list(c(0.1187257, 0.3012191, 0.04141983, 0.04143176, 0.3596399, 0.04900164, 0.08856198), c(251.8355, 248.3886, 256.9661, 400.3202, 249.8608, 282.2735, 1846.404), c(1.605597, 0.6552529, 3.766472, 95.78945, 0.9881022, 13.27742, 490.545))
model_128k_readdominant_write <- list(c(0.4806858, 0.07281363, 0.1359416, 0.03205336, 0.2764217, 0.002084018), c(264.6677, 298.0264, 268.3553, 272.4679, 266.8599, 772.3884), c(1.170143, 16.80264, 2.104416, 3.361579, 1.162535, 1720.683))
model_128k_writedominant_read <- list(c(0.0682443, 0.3324055, 0.1020772, 0.1220944, 0.007280993, 0.2752338, 0.09266368), c(482.7889, 379.4744, 379.6946, 400.6295, 3917.901, 2244.981, 2615.717), c(102.7631, 12.65709, 23.41827, 3.366796, 1243, 533.5585, 134.0154))
model_128k_writedominant_write <- list(c(0.0198333, 0.1908779, 0.3443286, 0.211707, 0.170983, 0.06126752, 0.00100271), c(262.2961, 266.0839, 267.6006, 267.5385, 281.7369, 265.2672, 1053.052), c(0.6904319, 0.7353543, 2.183362, 1.093442, 9.312287, 1.550056, 1308.625))
