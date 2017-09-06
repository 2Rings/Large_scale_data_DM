#2数据---------------------------------------------------------
type=c(0,1,0,1,0,1,1,1,1,1,0,0,0,1,1)
express=list("?",NA)
target="class"
name=c("age","workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","class")
file <- 'adult.test.txt'
data <- read.csv(file = file,header = F)
colnames(data) <- name

type=c(0,1,0,1,0,1,1,1,1,1,0,0,0,1,1)
express=list("?","",NA)
target="class"
name=c("age","workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","class")
file <- '2.csv'
data <- read.csv(file = file,header = F)
colnames(data) <- name

#20数据-----------------------------------------
type=c(0,1,0,1,0,1,1,1,1,1,0,0,0,1,1)
express=list("?")
target="class"
name=c("age","workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","class")
file <- 'adult.test.txt'
data <- read.csv(file = file,header = F)
colnames(data) <- name

type=c(0,1,0,1,0,1,1,1,1,1,0,0,0,1,1)
express=list("?")
target="class"
name=c("age","workclass","fnlwgt","education","education-num","marital-status","occupation","relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country","class")
file <- 'adult.data.txt'
data <- read.csv(file = file,header = F)
colnames(data) <- name

#54数据--------------------------------------------------------
type=c(rep(0,617),1)#从f1到f617都是连续型0，class是离散型1
express <- NULL
target="class"
name=c(paste('f',as.character(1:617),sep = ''),'class')
file <- 'isolet5.csv'
data <- read.csv(file = file,header = F)
colnames(data) <- name

type=c(rep(0,617),1)#从f1到f617都是连续型0，class是离散型1
express <- NULL
target="class"
name=c(paste('f',1:617,sep = ''),'class')
file <- 'isolet1+2+3+4.data'
data <- read.csv(file = file,header = F)
colnames(data) <- name

#59数据-------------------
type=c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
express <- NULL
target="lettr"
name=c("lettr","x-box","y-box","width","high","onpix","x-bar","y-bar","x2bar","y2bar","xybar","x2ybr","xy2br","x-ege","xegvy","y-ege","yegvx")
file <- 'letter-recognition.data.txt'
data <- read.csv(file = file,header = F)
colnames(data) <- name

#76数据-------------
type=c(1,1,1,1,1,1,1,1)
express <- NULL
target="health"
name=c("parents","has_nurs","form","children","housing","finance","social","health")
file <- 'nursery.data.txt'
data <- read.csv(file = file,header = F); data <- data[ ,-9]
colnames(data) <- name

#81数据--------------------------
type=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
express <- NULL
target="class"
name=c("input1","input2","input3","input4","input5","input6","input7","input8","input9","input10","input11","input12","input13","input14","input15","input16","class")
file <- 'pendigits.tes.txt'
data <- read.csv(file = file,header = F)
colnames(data) <- name

type=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
express <- NULL
target="class"
name=c("input1","input2","input3","input4","input5","input6","input7","input8","input9","input10","input11","input12","input13","input14","input15","input16","class")
file <- 'pendigits.tra.txt'
data <- read.csv(file = file,header = F)
colnames(data) <- name

#158数据------------------
#训练集
type=c(1,1,1,1,1,1,1,1,1,1,1)
express <- NULL
target="class"
name=c("S1","C1","S2","C2","S3","C3","S4","C4","S5","C5","class")
file <- 'poker-hand-training-true.csv'
data <- read.csv(file = file,header = F)
colnames(data) <- name

#测试集 运行不成功
type=c(1,1,1,1,1,1,1,1,1,1,1)
express <- NULL
target="class"
name=c("S1","C1","S2","C2","S3","C3","S4","C4","S5","C5","class")
file <- 'poker-hand-testing.csv'
data <- read.csv(file = file,header = F)
colnames(data) <- name

#159数据----------------
type=c(0,0,0,0,0,0,0,0,0,0,1)
express=NULL#无缺失
target="class"
name=c("fLength","fWidth","fSize","fConc","fConc1","fAsym","fM3Long","fM3Trans","fAlpha","fDist", "class")
file <- 'magic04.csv'
data <- read.csv(file = file,header = F)
colnames(data) <- name

#166数据-------------------------------------------------
#Hill_valley_with_noise_Testing.data
type=c(rep(0,100),1)
express=NULL#无缺失
target="class"
name=c(paste('x',1:100,sep = ''),"class")
file <- 'Hill_valley_with_noise_Testing.data'
data <- read.csv(file = file,header = T)
colnames(data) <- name

#Hill_valley_with_noise_Training.data
type=c(rep(0,100),1)
express=NULL#无缺失
target="class"
name=c(paste('x',1:100,sep = ''),"class")
file <- 'Hill_valley_with_noise_Training.data'
data <- read.csv(file = file,header = T)
colnames(data) <- name

#Hill_valley_without_noise_Testing.data
type=c(rep(0,100),1)
express=NULL#无缺失
target="class"
name=c(paste('x',1:100,sep = ''),"class")
file <- 'Hill_valley_without_noise_Testing.data'
data <- read.csv(file = file,header =T)
colnames(data) <- name

#Hill_valley_without_noise_Training.data
type=c(rep(0,100),1)
express=NULL#无缺失
target="class"
name=c(paste('x',1:100,sep = ''),"class")
file <- 'Hill_valley_without_noise_Training.data'
data <- read.csv(file = file,header = T)
colnames(data) <- name

#193数据------------------------
# 输入参数193 sensor_readings_24
type=c(rep(0,24),1)#24个连续的（0），最后一个是离散的（1）
express=NULL#无缺失
target="class"
name=c(paste('U',1:24,sep = ''),"class")
file <- 'sensor_readings_24.data'
data <- read.csv(file = file,header = T)
colnames(data) <- name

# 输入参数193 sensor_readings_4.data
type=c(0,0,0,0,1)
express=NULL#无缺失
target="class"
name=c("SD_front","SD_left","SD_right","SD_back","class")
file <- 'sensor_readings_4.data'
data <- read.csv(file = file,header = T)
colnames(data) <- name

# 输入参数193 sensor_readings_2.data
type=c(0,0,1)
express=NULL#无缺失
target="class"
name=c("SD_front","SD_left","class")
file <- 'sensor_readings_2.data'
data <- read.csv(file = file,header = T)
colnames(data) <- name

#199数据----数据太大无法运行-----------------------
type=c(1,rep(0,90))#一个1,90个0
express=NULL
target="year"
name=c("year",paste('timbreaverage',1:12,sep = ''),paste('timbrecovariance',1:78,sep = ''))
file <- 'YearPredictionMSD.csv'
data <- read.csv(file = file,header = F)
colnames(data) <- name

#202数据--数据太大无法运行-------------------------
type=c(1,rep(0,385))#第一个是patient ID，后面变量均是连续的，有385个O
express=NULL#无缺失
target="reference"
name=c("patientId",paste('value',0:383,sep = ''),"reference")
file <- 'slice_localization_data.csv'
data <- read.csv(file = file,header = T)
colnames(data) <- name

#205数据-------
 输入参数205 block_1.csv------------------------------------------
type=c(0,0,0,0,0,0,1,1,1,1,1,1)
express=list("?")
target="is_match"
name=c("id_1","id_2","cmp_fname_c1","cmp_fname_c2","cmp_lname_c1","cmp_lname_c2","cmp_sex","cmp_bd","cmp_bm","cmp_by","cmp_plz","is_match")
file <- 'block_1.csv'
data <- read.csv(file = file,header = T)
colnames(data) <- name

 输入参数205 block_2.csv------------------------------------------
type=c(0,0,0,0,0,0,1,1,1,1,1,1)
express=list("?")
target="is_match"
name=c("id_1","id_2","cmp_fname_c1","cmp_fname_c2","cmp_lname_c1","cmp_lname_c2","cmp_sex","cmp_bd","cmp_bm","cmp_by","cmp_plz","is_match")
file <- 'block_2.csv'
data <- read.csv(file = file,header = T)
colnames(data) <- name

 输入参数205 block_3.csv------------------------------------------
type=c(0,0,0,0,0,0,1,1,1,1,1,1)
express=list("?")
target="is_match"
name=c("id_1","id_2","cmp_fname_c1","cmp_fname_c2","cmp_lname_c1","cmp_lname_c2","cmp_sex","cmp_bd","cmp_bm","cmp_by","cmp_plz","is_match")
file <- 'block_3.csv'
data <- read.csv(file = file,header = T)
colnames(data) <- name

 输入参数205 block_4.csv------------------------------------------
type=c(0,0,0,0,0,0,1,1,1,1,1,1)
express=list("?")
target="is_match"
name=c("id_1","id_2","cmp_fname_c1","cmp_fname_c2","cmp_lname_c1","cmp_lname_c2","cmp_sex","cmp_bd","cmp_bm","cmp_by","cmp_plz","is_match")
file <- 'block_4.csv'
data <- read.csv(file = file,header = T)
colnames(data) <- name

 输入参数205 block_5.csv------------------------------------------
type=c(0,0,0,0,0,0,1,1,1,1,1,1)
express=list("?")
target="is_match"
name=c("id_1","id_2","cmp_fname_c1","cmp_fname_c2","cmp_lname_c1","cmp_lname_c2","cmp_sex","cmp_bd","cmp_bm","cmp_by","cmp_plz","is_match")
file <- 'block_5.csv'
data <- read.csv(file = file,header = T)
colnames(data) <- name

 输入参数205 block_6.csv------------------------------------------
type=c(0,0,0,0,0,0,1,1,1,1,1,1)
express=list("?")
target="is_match"
name=c("id_1","id_2","cmp_fname_c1","cmp_fname_c2","cmp_lname_c1","cmp_lname_c2","cmp_sex","cmp_bd","cmp_bm","cmp_by","cmp_plz","is_match")
file <- 'block_6.csv'
data <- read.csv(file = file,header = T)
colnames(data) <- name
 
 输入参数205 block_7.csv------------------------------------------
type=c(0,0,0,0,0,0,1,1,1,1,1,1)
express=list("?")
target="is_match"
name=c("id_1","id_2","cmp_fname_c1","cmp_fname_c2","cmp_lname_c1","cmp_lname_c2","cmp_sex","cmp_bd","cmp_bm","cmp_by","cmp_plz","is_match")
file <- 'block_7.csv'
data <- read.csv(file = file,header = T)
colnames(data) <- name

 输入参数205 block_8.csv------------------------------------------
type=c(0,0,0,0,0,0,1,1,1,1,1,1)
express=list("?")
target="is_match"
name=c("id_1","id_2","cmp_fname_c1","cmp_fname_c2","cmp_lname_c1","cmp_lname_c2","cmp_sex","cmp_bd","cmp_bm","cmp_by","cmp_plz","is_match")
file <- 'block_8.csv'
data <- read.csv(file = file,header = T)
colnames(data) <- name

 输入参数205 block_9.csv------------------------------------------
type=c(0,0,0,0,0,0,1,1,1,1,1,1)
express=list("?")
target="is_match"
name=c("id_1","id_2","cmp_fname_c1","cmp_fname_c2","cmp_lname_c1","cmp_lname_c2","cmp_sex","cmp_bd","cmp_bm","cmp_by","cmp_plz","is_match")
file <- 'block_9.csv'
data <- read.csv(file = file,header = T)
colnames(data) <- name

 输入参数205 block_10.csv------------------------------------------
type=c(0,0,0,0,0,0,1,1,1,1,1,1)
express=list("?")
target="is_match"
name=c("id_1","id_2","cmp_fname_c1","cmp_fname_c2","cmp_lname_c1","cmp_lname_c2","cmp_sex","cmp_bd","cmp_bm","cmp_by","cmp_plz","is_match")
file <- 'block_10.csv'
data <- read.csv(file = file,header = T)
colnames(data) <- name

#217数据------------------------------------------------------
 bank-full
type=c(0,1,1,1,1,0,1,1,1,1,1,0,0,0,0,1,1)
express=NULL
target="y"
name=c("age","job","marital","education","default","balance","housing","loan","contact","day","month","duration","campaign","pdays","previous","poutcome","y")
file <- 'bank-full.csv'
data <- read.csv(file = file,header = T)

  
 bank-additional-full----------------------
type=c(0,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,0,0,0,0,1)
express=NULL
target="y"
name=c("age","job","marital","education","default","housing","loan","contact","month","day_of_week","duration","campaign","pdays","previous","poutcome","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed","y")
file <- 'bank-additional-full.csv'
data <- read.csv(file = file,header = T)

#222数据-------------------------------------------------------
type=c(1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,0,0,0,1,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,1)
express=list("?")
target="label"
name=c("id","clean_name_intersect_min","clean_name_intersect_max","clean_name_levenshtein_sim","clean_name_trigram_sim","clean_name_levenshtein_term","clean_name_trigram_term","clean_name_including","clean_name_equality","city_intersect_min","city_intersect_max","city_levenshtein_sim","city_trigram_sim","city_levenshtein_term","city_trigram_term","city_including","city_equality","zip_intersect_min","zip_intersect_max","zip_levenshtein_sim","zip_trigram_sim","zip_levenshtein_term","zip_trigram_term","zip_including","zip_equality","street_intersect_min","street_intersect_max","street_levenshtein_sim","street_trigram_sim","street_levenshtein_term","street_trigram_term","street_including","street_equality","website_intersect_min","website_intersect_max","website_levenshtein_sim","website_trigram_sim","website_levenshtein_term","website_trigram_term","website_including","website_equality","countryname_intersect_min","countryname_intersect_max","countryname_levenshtein_sim","countryname_trigram_sim","countryname_levenshtein_term","countryname_trigram_term","countryname_including","countryname_equality","geocoderlocalityname_intersect_min","geocoderlocalityname_intersect_max","geocoderlocalityname_levenshtein_sim","geocoderlocalityname_trigram_sim","geocoderlocalityname_levenshtein_term","geocoderlocalityname_trigram_term","geocoderlocalityname_including","geocoderlocalityname_equality","geocoderinputaddress_intersect_min","geocoderinputaddress_intersect_max","geocoderinputaddress_levenshtein_sim","geocoderinputaddress_trigram_sim","geocoderinputaddress_levenshtein_term","geocoderinputaddress_trigram_term","geocoderinputaddress_including","geocoderinputaddress_equality","geocoderoutputaddress_intersect_min","geocoderoutputaddress_intersect_max","geocoderoutputaddress_levenshtein_sim","geocoderoutputaddress_trigram_sim","geocoderoutputaddress_levenshtein_term","geocoderoutputaddress_trigram_term","geocoderoutputaddress_including","geocoderoutputaddress_equality","geocoderpostalcodenumber_intersect_min","geocoderpostalcodenumber_intersect_max","geocoderpostalcodenumber_levenshtein_sim","geocoderpostalcodenumber_trigram_sim","geocoderpostalcodenumber_levenshtein_term","geocoderpostalcodenumber_trigram_term","geocoderpostalcodenumber_including","geocoderpostalcodenumber_equality","geocodercountrynamecode_intersect_min","geocodercountrynamecode_intersect_max","geocodercountrynamecode_levenshtein_sim","geocodercountrynamecode_trigram_sim","geocodercountrynamecode_levenshtein_term","geocodercountrynamecode_trigram_term","geocodercountrynamecode_including","geocodercountrynamecode_equality","phone_diff","phone_levenshtein","phone_trigram","phone_equality","fax_diff","fax_levenshtein","fax_trigram","fax_equality","street_number_diff","street_number_levenshtein","street_number_trigram","street_number_equality","geocode_coordinates_long_diff","geocode_coordinates_long_levenshtein","geocode_coordinates_long_trigram","geocode_coordinates_long_equality","geocode_coordinates_lat_diff","geocode_coordinates_lat_levenshtein","geocode_coordinates_lat_trigram","geocode_coordinates_lat_equality","coordinates_long_diff","coordinates_long_levenshtein","coordinates_long_trigram","coordinates_long_equality","coordinates_lat_diff","coordinates_lat_levenshtein","coordinates_lat_trigram","coordinates_lat_equality","geocode_coordinates_diff","coordinates_diff","label")
file <- 'Nomao.csv'
data <- read.csv(file = file,header =F)
colnames(data) <- name

#224数据------------------------------------------------
type=c(0,0,0,1)
express=NULL
target="label"
name=c("B","G","R","label")
file <- 'Skin_NonSkin.csv'
data <- read.csv(file = file,header = F)
colnames(data) <- name

#257数据-------------------------------------------------
type=c(0,0,0,0,0,0,0,0,0,0)
express=NULL
target="RMSD"
name=c("RMSD","F1","F2","F3","F4","F5","F6","F7","F8","F9")
file <- 'CASP.csv'
data <- read.csv(file = file,header =T)

#279数据----------------
type=c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
express=NULL
target="Class"
name=c("Class","Specimen_Number","Eccentricity","Aspect_Ratio","Elongation","Solidity","Stochastic_Convexity","Isoperimetric_Factor","Maximal_Indentation_Depth","Lobedness","Average_Intensity","Average_Contrast","Smoothness","Third_moment","Uniformity","Entropy")
file <- 'leaf.csv'
data <- read.csv(file = file,header =F)
colnames(data) <- name

#285数据------------
type=c(0,0,0,0,0)
express=NULL
target="PE"
name=c("AT","V","AP","RH","PE")
file <- 'Folds5x2_pp.csv'
data <- read.csv(file = file,header = T)

#294数据----------------------------
blogData_train

type=c(rep(0,62),rep(1,214),rep(0,5))#62个0，214个1，5个0，共281个变量
express=NULL
target="target"
name=c(paste("V",1:280,sep=""),"target")
file <- 'blogData_train.csv'
data <- read.csv(file = file,header =F)
colnames(data) <- name

blogData_test

type=c(rep(0,62),rep(1,214),rep(0,5))#62个0，214个1，5个0，共281个变量
express=NULL
target="target"
name=c(paste("V",1:280,sep=""),"target")
file <- 'blogData_test.csv'
data <- read.csv(file = file,header =F)
colnames(data) <- name

#拍拍贷数据-----------
express <- list('',' ',-1,'','不详',NA)
type <- c(1, 1 ,1 ,1, 0 ,0, 0, 0, 0, 0, 0 ,0 ,0 ,0 ,0 ,0, 0, 0, 0, 0, 0 ,0 ,1 ,1 ,1, 1, 1, 0 ,1, 1 ,1 ,1, 1,
          1, 1, 0, 1, 1 ,1 ,1 ,1, 1 ,1 ,1, 1, 1 ,1, 1 ,1 ,1 ,1, 1, 1 ,0, 0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0, 0, 0, 0, 0,
          0, 0, 0, 0 ,0, 0 ,0, 0 ,0 ,0, 0 ,0 ,0 ,0 ,0 ,0 ,0 ,0, 0 ,0, 0 ,0 ,0 ,0 ,0 ,0, 0 ,0 ,0, 0, 0, 0 ,0, 0,
          0 ,0, 0, 0, 0 ,0 ,0 ,0 ,0, 0, 0 ,0, 0, 0, 0 ,0 ,0 ,0, 0 ,0, 0 ,0 ,0, 0, 0 ,0 ,0, 0, 0 ,0 ,0, 0, 0, 0,
          0, 0 ,0 ,0, 0 ,0, 0, 0 ,0, 0, 0 ,0, 0 ,0, 0, 0, 0 ,0, 0 ,0, 0, 0 ,0, 0 ,0 ,0 ,0 ,0, 0, 0 ,0 ,0, 0 ,0,
          0, 0, 0 ,0 ,0 ,0, 0, 0 ,0, 0, 0, 0, 0 ,0 ,0 ,0, 0 ,0 ,0 ,0 ,0, 0 ,0 ,0 ,0 ,0 ,0 ,0, 0 ,0 ,0 ,0, 0 ,0,
          0 ,0, 0 ,0, 0 ,1, 1 ,0, 0 ,0, 0 ,1 ,0 ,0 ,0 ,0, 1 ,0, 0, 0, 0, 0 ,1)
target='target'
file <- 'PPD_Training_Master_GBK_3_1_Training_Set.csv'
data <- read.csv(file = file,header = T)



#处理---------------------------------------------------
data_preprocess(data,type,express,target,out = T)

category_to_numeric(train_data, test_data,target,type, out = T)

write.csv(target,file='target.csv',row.names = F)
write.csv(test_data,file='test_data.csv',row.names = F)
write.csv(train_data,file='train_data.csv',row.names = F)
write.csv(type,file='type.csv',row.names = F)

#运行DM程序-----------------------------
t1<-Sys.time()
DM(data, type, express, target,target_order = F)
Sys.time()-t1
