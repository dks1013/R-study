#예측하기 gre 376, gpa 3.6이고 rank를 1,2,3,4로 바꿔가면서 합격 확률을 출력하는 프로그램 작성하세요.
#for문 사용
ucla <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

m <- glm(admit ~., data = ucla, family = binomial)
i <- 1
for (i in 1:4){
  s <- data.frame(rbind(c(376, 3.6, i)))
  names(ucla)
  names(s) <- names(ucla)[2:4]
  print(predict(m, s, type = 'response'))
}
