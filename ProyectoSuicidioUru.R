library(gmodels)
library(MASS)
library(carData)
library(car)
library(zoo)
library(lmtest)
library(readxl)
library(MASS)
library(rsm)
library(xtable)
library(dplyr)
library(haven)

UYH2012 = as.data.frame(read_sav("UYH2012_Public_Use.sav"))
Datos = UYH2012[,c(1,2,4,5,24,25,69,73,75,76,77,78,80,88,97,106,109,110)]
Datos
Datos$Q1 <- as.factor(Datos$Q1);levels(Datos$Q1)<-c("11_AÑOS_menos","12_AÑOS","13_AÑOS","14_AÑOS","15_AÑOS","16_AÑOS_O_MAS")
Datos$Q2 <- as.factor(Datos$Q2);levels(Datos$Q2)<-c("Masculino","Femenino")
Datos$Q25 <- as.factor(Datos$Q25);levels(Datos$Q25)<-c("Si","No")
Datos$Q24 <- as.factor(Datos$Q24);levels(Datos$Q24)<-c("Si","No")
Datos$QN16 <- as.factor(Datos$QN16);levels(Datos$QN16)<-c("Si","No")
Datos$QN20 <- as.factor(Datos$QN20);levels(Datos$QN20)<-c("Si","No")
Datos$QN22 <- as.factor(Datos$QN22);levels(Datos$QN22)<-c("Si","No")
Datos$QN23 <- as.factor(Datos$QN23);levels(Datos$QN23)<-c("Si","No")
Datos$QN24 <- as.factor(Datos$QN24);levels(Datos$QN24)<-c("Si","No")
Datos$QN25 <- as.factor(Datos$QN25);levels(Datos$QN25)<-c("Si","No")
Datos$QN27 <- as.factor(Datos$QN27);levels(Datos$QN27)<-c("Si","No")
Datos$QN35 <- as.factor(Datos$QN35);levels(Datos$QN35)<-c("Si","No")
Datos$QN44 <- as.factor(Datos$QN44);levels(Datos$QN44)<-c("Si","No")
Datos$QN53 <- as.factor(Datos$QN53);levels(Datos$QN53)<-c("Si","No")
Datos$QN56 <- as.factor(Datos$QN56);levels(Datos$QN56)<-c("Si","No")
Datos$QN57 <- as.factor(Datos$QN57);levels(Datos$QN57)<-c("Si","No")


####################################################################
# Analisis descriptivo
#####################################################################
##### independencia cualitativas
Datos2 = Datos[,c(1,2,6,7,8,9,10,13,14,15,16,17)]
Datos2 = na.omit(Datos2)
Datos2
Pchi <- matrix(0,ncol=12,nrow=12)
for(i in 1:12){
    for(j in 1:12){
        Pchi[i,j]=chisq.test(table(Datos2[,i],Datos2[,j]),simulate.p.value = T)$p.value
    }
}
Pchi
library(xtable)
xtable(Pchi)
Datos3 = Datos[,c(1,2,3,4,6,8,9,10,12,13,14,15,16,17)]
Datos3 = na.omit(Datos3)
##### independencia cuantitativas con la variable respuesta
attach(Datos3)
t.test(Q4~QN20)
t.test(Q5~QN20)
###############################################
# Creacion del modelo
##############################################

###############################
#Modelo con todas las variables
##############################

modelo1<-glm(QN25~Q1+Q2+Q4+Q5+QN20+QN22+QN23+QN27+QN35+QN44+QN53+QN56, family = binomial,data = Datos3)
step.model1 <- stepAIC(modelo1, direction = "both",trace = FALSE)
summary(step.model1) # AIC =1525.3

###### Interacciones
modelo11 = glm(QN25 ~ Q2*QN20*QN22*QN23*QN35*QN44* 
                  QN53*QN56, family = binomial, data = Datos3)
summary(modelo11) # AIC =25438

######### Modelo con interacciones 
modelo3= glm(QN25 ~Q1+Q2+Q4+Q5+QN20+QN22+QN23+QN27+QN35+QN44+QN53+QN56+Q2*QN20+Q2*QN23+
                 
                 QN20:QN44+QN22:QN44+QN23:QN44+QN35:QN44+QN20:QN53+QN22:QN53+QN23:QN53+QN35:QN53+QN44:QN53                                                                                      
             +QN20:QN56+QN22:QN56+QN23:QN56+QN35:QN56+QN44:QN56+QN53:QN56+QN20:QN35+QN22:QN35+QN23:QN35+QN20:QN23+QN22:QN23, family = binomial, data = Datos3)
step.model2 <- stepAIC(modelo3, direction = "both",trace = FALSE)
summary(step.model2)
# Modelo con interacciones significativas

modelo2 = glm( QN25 ~ Q2 + QN20 + QN22 + QN23 + QN35 + QN44 + 
                   QN53 + QN56 + Q2:QN20 + QN20:QN44 + QN22:QN44 + QN20:QN53 + 
                   QN23:QN53 + QN35:QN53 + QN22:QN23, family = binomial, 
               data = Datos3)
summary(modelo2) # AIC = 1513.3
#################################################################################
## Macros
envelope_glm <- function(fit.model, rep, conf, xlab, ylab, main, pch, col, identify, type){
    oldw <- getOption("warn")
    options(warn = -1)
    
    if(missingArg(type)) type <- "deviance"
    if(type != "deviance" & type != "pearson") 
        stop("Only deviance- and pearson-type residuals are supported!!",call.=FALSE)
    if(missingArg(rep))   rep=50
    if(missingArg(conf))   conf=0.95
    if(conf<0 || conf>1) stop("Only values of the argument 'conf' within the (0,1) interval are allowed!!",call.=FALSE)
    if(rep<0) stop("Only positive values of the argument 'rep' are allowed!!",call.=FALSE)
    
    if(class(fit.model)[1] != "glm" & class(fit.model)[1] != "lm") 
        stop("Only lm- and glm-type objects are suported!!",call.=FALSE)
    X <- model.matrix(fit.model)
    n <- nrow(X)
    p <- ncol(X)
    if(class(fit.model)[1] == "glm") w <- fit.model$weights
    if(class(fit.model)[1] == "lm"){
        if(is.null(fit.model$weights)) w <- matrix(1,n,1)
        else w <- fit.model$weights
    }  
    Xw <- X*matrix(sqrt(w),n,p)
    H <- Xw%*%tcrossprod(solve(crossprod(Xw,Xw)),Xw)
    h <- diag(H)
    e <- matrix(0,n,floor(rep))
    if(class(fit.model)[1] == "glm"){
        rig <- function(n,mu,phi){
            y <- rnorm(n)^2
            x <- mu + mu^2*y/(2*phi) - (mu/(2*phi))*sqrt(4*mu*phi*y + mu^2*y^2)
            u <- runif(n)
            ifelse(u <= mu/(mu + x),x,mu^2/x)
        }
        if(fit.model$family$family!="gaussian" & fit.model$family$family!="Gamma" &
           fit.model$family$family!="inverse.gaussian" & fit.model$family$family!="poisson" & 
           fit.model$family$family!="binomial") 
            stop("Only gaussian, Gamma, inverse.gaussian, poisson and binomial families are supported!!",call.=FALSE)
        ts <- resid(fit.model,type="pearson")
        phi <- (n-p)/sum(ts^2)
        if(fit.model$family$family=="poisson" || fit.model$family$family=="binomial") phi <- 1
        td <- resid(fit.model,type=type)*sqrt(phi/(1-h))
        bar <- txtProgressBar(min=0, max=rep, initial=0, width=min(50,rep), char="+", style=3)
        i <- 1
        while(i<=floor(rep)){
            if(fit.model$family$family=="Gamma"){
                resp <- rgamma(n,phi)
                resp <- (fitted(fit.model)/phi)*resp
            }
            if(fit.model$family$family=="inverse.gaussian") resp <- rig(n,fitted(fit.model),phi)
            if(fit.model$family$family=="gaussian") resp <- sqrt(1/phi)*rnorm(n,0,1) + fitted(fit.model)
            if(fit.model$family$family=="poisson") resp <- rpois(n, fitted(fit.model))
            if(fit.model$family$family=="binomial"){
                if(ncol(as.matrix(fit.model$model[,1]))==1){
                    resp <- runif(n)
                    resp <- resp <= fitted(fit.model)
                }
                else{ntot <- apply(fit.model$model[,1],1,sum)
                resp <- matrix(0,length(ntot),1)
                probs <- fitted(fit.model)
                for(j in 1:length(ntot)) resp[j] <- rbinom(1,ntot[j],probs[j])
                resp <- cbind(resp,ntot-resp)
                }	
            }
            fit <- try(glm(resp ~ -1 + X, family=fit.model$family, offset=fit.model$offset, start=coef(fit.model)),silent=TRUE)
            if(is.list(fit)){
                if(fit$converged==TRUE){
                    w <- fit$weights
                    Xw <- X*matrix(sqrt(w),n,p)
                    h <- diag(Xw%*%tcrossprod(solve(crossprod(Xw,Xw)),Xw))
                    phi <- (n-p)/sum(resid(fit,type="pearson")^2)
                    if(fit$family$family=="poisson" || fit$family$family=="binomial") phi <- 1
                    e[,i] <- sort(resid(fit,type=type)*sqrt(phi/(1-h)))
                    setTxtProgressBar(bar,i)
                    i <- i + 1
                }	
            }
        }
    }
    if(class(fit.model)[1] == "lm"){
        ts <- resid(fit.model)
        phi <- (n-p)/sum(ts^2*w)
        td <- resid(fit.model)*sqrt(phi*w/(1-h))
        td <- td*sqrt((n-p-1)/(n-p-td^2))
        e <- matrix(0,n,rep)
        bar <- txtProgressBar(min=0, max=rep, initial=0, width=min(50,rep), char="+", style=3)
        i <- 1
        while(i<=floor(rep)){
            err <- as.matrix(rnorm(n))
            ress <- (err - H%*%err)/sqrt(w)
            phis <- (n-p)/sum((ress)^2*w)
            tds <- ress*sqrt(phis*w/(1-h))
            tds <- tds*sqrt((n-p-1)/(n-p-tds^2))
            e[,i] <- sort(tds)
            setTxtProgressBar(bar,i)
            i <- i + 1
        }	
    }
    e1 <- matrix(0,n,1)
    e2 <- matrix(0,n,1)
    alpha <- 1 - conf
    for(i in 1:n){
        eo <- sort(e[i,])
        e1[i] <- quantile(eo,alpha/2)
        e2[i] <- quantile(eo,1-alpha/2)}
    med <- apply(e,1,mean)
    faixa <- range(td,e1,e2)
    par(pty="s")
    if(missingArg(xlab))  xlab="Expectation of order statistics of N(0,1)"
    if(missingArg(ylab))  ylab=paste("Standardized ",type,"-type residuals")
    if(class(fit.model)[1] == "lm") ylab="Standardized residuals"
    if(missingArg(main))  main=" "
    if(missingArg(pch))   pch=20
    if(missingArg(col))   col="black"
    close(bar)
    par(pty="s")
    outm <- qqnorm(td, xlab="", ylab="", ylim=faixa, main=main, pch=pch, col=col)
    par(new=TRUE)
    qqnorm(e1,axes=FALSE,xlab="",ylab="",main="", type="l",ylim=faixa,lty=1)
    par(new=TRUE)
    qqnorm(e2,axes=FALSE,xlab="",ylab="", main="", type="l",ylim=faixa,lty=1)
    par(new=TRUE)
    qqnorm(med,axes=FALSE,xlab=xlab,ylab=ylab,main=main,type="l",ylim=faixa,lty=2)
    if(!missingArg(identify)) identify(outm$x,outm$y, n=identify)
    
    options(warn = oldw)
}


residuals_glm <- function(fit.model, xlab, ylab, main, pch, col, identify, type){
    if(class(fit.model)[1] != "glm" & class(fit.model)[1] != "lm") 
        stop("Only lm- and glm-type objects are suported!!",call.=FALSE)
    if(missingArg(type)) type <- "deviance"
    if(type != "deviance" & type != "pearson") 
        stop("Only deviance- and pearson-type residuals are supported!!",call.=FALSE)
    
    X <- model.matrix(fit.model)
    n <- nrow(X)
    p <- ncol(X)
    if(class(fit.model)[1] == "glm") w <- fit.model$weights
    if(class(fit.model)[1] == "lm"){
        if(is.null(fit.model$weights)) w <- matrix(1,n,1)
        else w <- fit.model$weights
    }  
    Xw <- X*matrix(sqrt(w),n,p)
    h <- diag(Xw%*%tcrossprod(solve(crossprod(Xw,Xw)),Xw))
    if(class(fit.model)[1] == "glm"){
        ts <- resid(fit.model,type="pearson")
        phi <- (n-p)/sum(ts^2)
        if(fit.model$family$family=="poisson" || fit.model$family$family=="binomial") phi <- 1
        td <- resid(fit.model,type=type)*sqrt(phi/(1-h))
    }        
    if(class(fit.model)[1] == "lm"){
        ts <- resid(fit.model)
        phi <- (n-p)/sum(ts^2*w)
        td <- resid(fit.model)*sqrt(phi*w/(1-h))
        td <- td*sqrt((n-p-1)/(n-p-td^2))
    }        
    muhat <- fitted(fit.model)
    if(missingArg(xlab))  xlab="Fitted values"
    if(missingArg(ylab))  ylab=paste("Standardized ",type,"-type residuals")
    if(class(fit.model)[1] == "lm") ylab="Standardized residuals"
    if(missingArg(main))  main=" "
    if(missingArg(pch))   pch=20
    if(missingArg(col))   col="black"
    minx <- min(-3.5,min(td))
    maxx <- max(+3.5,max(td))
    plot(muhat,td,ylim=c(minx,maxx),xlab=xlab,ylab=ylab,main=main,col=col,pch=pch)
    abline(h=-3,lty=3)  
    abline(h=+3,lty=3)
    if(!missingArg(identify)) identify(muhat,td, n=identify)
    y=data.frame(minx,maxx,muhat)
    return(y)
}

Cookdis_glm <- function(fit.model, xlab, ylab, main, pch, col, identify){
    if(class(fit.model)[1] != "glm" & class(fit.model)[1] != "lm") 
        stop("Only lm- and glm-type objects are suported!!",call.=FALSE)
    X <- model.matrix(fit.model)
    n <- nrow(X)
    p <- ncol(X)
    if(class(fit.model)[1] == "glm") w <- fit.model$weights
    if(class(fit.model)[1] == "lm"){
        if(is.null(fit.model$weights)) w <- matrix(1,n,1)
        else w <- fit.model$weights
    }  
    Xw <- X*matrix(sqrt(w),n,p)
    h <- diag(Xw%*%tcrossprod(solve(crossprod(Xw,Xw)),Xw))
    if(class(fit.model)[1] == "glm"){
        ts <- resid(fit.model,type="pearson")
        phi <- (n-p)/sum(ts^2)
        if(fit.model$family$family=="poisson" || fit.model$family$family=="binomial") phi <- 1
        td <- resid(fit.model,type="pearson")*sqrt(phi/(1-h))
    }        
    if(class(fit.model)[1] == "lm"){
        ts <- resid(fit.model)
        phi <- (n-p)/sum(ts^2*w)
        td <- resid(fit.model)*sqrt(phi*w/(1-h))
    }
    dc <- (h/(1-h))*(td^2)
    if(missingArg(xlab))  xlab="Index"
    if(missingArg(ylab))  ylab="Cook distance"
    if(missingArg(main))  main=" "
    if(missingArg(pch))   pch=20
    if(missingArg(col))   col="black"
    plot(dc, xlab=xlab, ylab=ylab, main=main, pch=pch, col=col, type="h")
    if(!missingArg(identify)) identify(dc, n=identify)
    return(stat=dc)
}

GOF_glm <- function(fit.model){
    if(class(fit.model)[1] != "glm" & class(fit.model)[1] !="lm") 
        stop("Only lm- and glm-type objects are suported!!",call.=FALSE)
    X <- model.matrix(fit.model)
    n <- nrow(X)
    p <- ncol(X)
    if(class(fit.model)[1] == "glm"){
        ts <- resid(fit.model,type="pearson")
        pearsonX2 <- sum(ts^2)
        if(fit.model$family$family=="gaussian" || fit.model$family$family=="Gamma" ||
           fit.model$family$family=="inverse.gaussian" || fit.model$family$family=="poisson" || 
           fit.model$family$family=="binomial"){ 
            ll <- as.numeric(logLik(fit.model))
            if(names(coef(fit.model))[1]=="(Intercept)"){
                pseudoR2 <- 1 - fit.model$deviance/fit.model$null.deviance
                cmat <- rbind(n-p, n-p, NA, NA, NA, NA)
                cmat2 <- rbind(pearsonX2, deviance(fit.model), pseudoR2, -2*ll, AIC(fit.model), BIC(fit.model))
                rownames(cmat) <- c("Pearson's Chi-squared   ", "Deviance", "Likelihood-based R-squared   ","-2logLikelihood", "AIC", "BIC")
            }else{
                cmat <- rbind(n-p, n-p, NA, NA, NA)
                cmat2 <- rbind(pearsonX2, deviance(fit.model), -2*ll, AIC(fit.model), BIC(fit.model))
                rownames(cmat) <- c("Pearson's Chi-squared   ", "Deviance", "-2logLikelihood", "AIC", "BIC")
            }
        }
        else{
            sigma2 <- pearsonX2/(n-p)
            QIC <- deviance(fit.model)/sigma2 + 2*p
            cmat <- rbind(n-p, NA)
            cmat2 <- rbind(deviance(fit.model), QIC)
            rownames(cmat) <- c("Deviance", "QIC")
        }
        cmat <- cbind(cmat,cmat2)
        colnames(cmat) <- c("df","   Value")
        printCoefmat(cmat,digits=10,na.print="")
    }
    if(class(fit.model)[1] == "lm"){
        if(is.null(fit.model$weights)) w <- matrix(1,n,1)
        else w <- as.matrix(fit.model$weights)
        ys <- resid(fit.model) + fitted(fit.model)
        Rss <- sum(resid(fit.model)^2*w)
        n <- length(ys)
        if(names(coef(fit.model))[1]=="(Intercept)"){
            mmn <- sum(ys*w)/sum(w) 
            r2 <- 1 - Rss/sum((ys-mmn)^2*w)
            r2a <- 1 - (1-r2)*((n-1)/(n-length(coef(fit.model))))    
        }
        if(names(coef(fit.model))[1]!="(Intercept)"){
            r2 <- 1 - Rss/(sum(ys^2*w))
            r2a <- 1 - (1-r2)*(n/(n-length(coef(fit.model))))    
        }
        Xw <- X*matrix(sqrt(w),n,p)
        h <- diag(Xw%*%tcrossprod(solve(crossprod(Xw,Xw)),Xw))
        PRESS <- sum((resid(fit.model)*sqrt(w)/(1-h))^2)
        cmat <- rbind(r2,r2a,PRESS,Rss/((n-p)*(n-p-2)),AIC(fit.model), BIC(fit.model))
        ### Mary L. Thompson International Statistical Review / Revue Internationale de Statistique
        ### Vol. 46, No. 1 (Apr., 1978), pp. 1-19
        rownames(cmat) <- c("Multiple R-squared   ", "Adjusted R-squared   ", "Allen's PRESS", "Thompson's Sp", "AIC", "BIC")
        colnames(cmat) <- c("   Value")
        printCoefmat(cmat,digits=10)
    }
}

case.deletion_glm <- function(fit.model,subset){
    if(class(fit.model)[1] != "glm" & class(fit.model)[1] != "lm") 
        stop("Only lm- and glm-type objects are suported!!",call.=FALSE)
    if(missingArg(subset)) 
        stop("A subset of individuals must be specified!!",call.=FALSE)
    X <- model.matrix(fit.model)
    n <- nrow(X)
    p <- ncol(X)
    cfit1 <- coef(fit.model)
    sefit1 <- sqrt(diag(vcov(fit.model)))
    pfit1 <- coef(fit.model)/sefit1
    pvalue  <- " Pr(>|t|) "
    pvalue2 <- " Pr(>|t|)*"
    if(class(fit.model)[1] == "lm"){
        y <- resid(fit.model) + fitted(fit.model)
        fit0 <- lm(y ~ -1 + X, offset=fit.model$offset, weights=fit.model$weights, subset=subset)
        cfit0 <- coef(fit0)
        sefit0 <- sqrt(diag(vcov(fit0)))
        pfit0 <- coef(fit0)/sefit0
        pfit0 <- 2*pt(-abs(pfit0),nrow(model.matrix(fit0))-p)
        pfit1 <- 2*pt(-abs(pfit1),n-p)
    }
    if(class(fit.model)[1] == "glm"){
        if(ncol(as.matrix(fit.model$model[,1]))==1) y <- fit.model$y
        else y <- as.matrix(fit.model$model[,1])
        fit0 <- glm(y ~ -1 + X, family=fit.model$family, offset=fit.model$offset, subset=subset)
        cfit0 <- coef(fit0)
        sefit0 <- sqrt(diag(vcov(fit0)))
        pfit0 <- coef(fit0)/sefit0
        if(fit.model$family$family!="binomial" & fit.model$family$family!="poisson"){
            pfit1 <- 2*pt(-abs(pfit1),n-p)
            pfit0 <- 2*pt(-abs(pfit0),nrow(model.matrix(fit0))-p)
        }else{
            pfit1 <- 2*pnorm(-abs(pfit1))
            pfit0 <- 2*pnorm(-abs(pfit0))
            pvalue  <- " Pr(>|z|) "
            pvalue2 <- " Pr(>|z|)*"
        }
    }
    varc <- 100*(cfit0-cfit1)/abs(cfit1)
    lims <- rep(NA,length(cfit1))
    cmat <- cbind(cfit1,sefit1,pfit1,lims,cfit0,sefit0,pfit0,lims,varc)
    rownames(cmat) <- names(cfit1)
    colnames(cmat) <- c("Estimate ","Std. Error ",pvalue,"|","Estimate*","Std. Error*",pvalue2,"|","Change(%)")
    cat("\n")
    printCoefmat(cmat,digits=4,cs.ind=c(1,2,5,6), tst.ind=c(3,7), zap.ind=9, dig.tst=5, na.print="|")
    cat("\n(*) estimates, standard errors and p-values obtained using the specified subset of individuals.\n")
}


vif_glm <- function(fit.model){
    if(class(fit.model)[1] != "glm" & class(fit.model)[1] != "lm") 
        stop("Only lm- and glm-type objects are suported!!",call.=FALSE)
    
    X <- model.matrix(fit.model)
    postos <-  attr(X,"assign")
    vars <- attr(fit.model$terms,"term.labels")
    n <- nrow(X)
    p <- ncol(X)
    if(class(fit.model)[1] == "glm"){
        w <- fit.model$weights
    }  
    if(class(fit.model)[1] == "lm"){
        postos <- fit.model$assign
        if(is.null(fit.model$weights)) w <- matrix(1,n,1)
        else w <- fit.model$weights
    }
    X <- X*matrix(sqrt(w),n,p)
    vcovar <- solve(t(X)%*%X)	
    if(names(coef(fit.model))[1]=="(Intercept)"){
        vcovar <- vcovar[-1,-1]
        postos <- postos[-1]
    }
    nn <- max(postos)
    if(nn==1) stop("at least two effects are required to calculate GVIFs!!",call.=FALSE)	
    result <- matrix(0,nn,3)
    cors <- cov2cor(vcovar)
    detx <- det(cors)
    for(i in 1:nn){
        rr2 <- as.matrix(cors[postos==i,postos==i])
        rr3 <- as.matrix(cors[postos!=i,postos!=i])
        result[i,1] <- det(rr2)*det(rr3)/detx
        result[i,2] <- ncol(rr2)
        result[i,3] <- result[i,1]^(1/(2*ncol(rr2)))
    }
    rownames(result) <- vars
    colnames(result) <- c("GVIF", "df", "GVIF^(1/(2*df))")
    printCoefmat(result,digits=5,tst.ind=2,dig.tst=0)
}

vdtest <- function(fit.model){
    if(class(fit.model)[1] != "glm" & class(fit.model)[1] != "lm") 
        stop("Only lm- and glm-type objects are suported!!",call.=FALSE)
    if(fit.model$family$family!="gaussian" & fit.model$family$family!="Gamma" &
       fit.model$family$family!="inverse.gaussian") 
        stop("Only gaussian, Gamma and inverse.gaussian families are supported!!",call.=FALSE)
    Z <- model.matrix(fit.model)  
    if(names(coef(fit.model))[1]=="(Intercept)") Z <- as.matrix(Z[,-1])
    n <- nrow(Z)
    p <- ncol(Z)
    Z_star <- matrix(0,n,p)
    for(i in 1:p) Z_star[,i] <- Z[,i]-mean(Z[,i])
    H_star <- Z_star%*%solve(t(Z_star)%*%Z_star)%*%t(Z_star)
    if(class(fit.model)[1] == "lm"){
        t <- -(resid(fit.model)^2)/2
        phies <- -1/(2*mean(t))
        u <- -2*t*phies - 1
    }  
    if(class(fit.model)[1] == "glm"){
        y <- fit.model$y
        mus <- fitted(fit.model)
        if(fit.model$family$family=="gaussian"){
            t <- -(y-mus)^2/2
            phies <- -1/(2*mean(t))
            u <- -2*t*phies - 1
        }
        if(fit.model$family$family=="inverse.gaussian"){
            t <- -(y/(2*mus^2) + 1/mus + 1/(2*mus))
            phies <- -1/(2*mean(t))
            u <- -2*t*phies - 1
        }
        if(fit.model$family$family=="Gamma"){
            t <- log(y/mus) - y/mus
            phies <- (n-p)/sum(resid(fit.model,type="pearson")^2)
            phies_min <- phies*0.6
            phies_max <- phies*1.4		  
            phies <- uniroot(function(x) psigamma(x) - 1 - log(x) - mean(t), lower=phies_min, upper=phies_max)$root
            u <- sqrt(2)*(- t - 1 - log(phies) + psigamma(phies))/sqrt(psigamma(phies,1) - 1/phies)
        }  
    }
    SC = (1/2)*t(u)%*%H_star%*%u
    cat("\n             Score test for varying dispersion\n\n")
    cat("Statistic = ",round(SC,digits=5),",  df = ",p,",  p-value = ",format.pval(1-pchisq(SC,p)),"\n\n")	
}


dC2 <- function(fit.model, xlab.mu, xlab.phi, ylab.mu, ylab.phi, main.mu, main.phi, pch, identify.mu, identify.phi){
    if(class(fit.model)[1] != "dglm") stop("Only dglm-type objects are suported!!",call.=FALSE)
    
    if(missingArg(xlab.mu))  xlab.mu="Indice"
    if(missingArg(ylab.mu))  ylab.mu="Distancia de Cook"
    if(missingArg(main.mu))  main.mu="Location parameters"
    if(missingArg(xlab.phi)) xlab.phi="Indice"
    if(missingArg(ylab.phi)) ylab.phi="Distancia de Cook"
    if(missingArg(main.phi)) main.phi="Dispersion parameters"
    if(missingArg(pch))   pch=16
    
    Z <- model.matrix(fit.model$dispersion)
    n <- nrow(Z)
    q <- ncol(Z)
    X <- model.matrix(fit.model)
    p <- ncol(X)
    mu <- fitted(fit.model)
    resp <- fit.model$y
    fi <- fitted(fit.model$dispersion)
    fi <- 1/fi
    w <- fit.model$weights
    w <- w*fi
    X <- matrix(sqrt(w),n,p)*X
    H <- solve(t(X)%*%X)
    h <- diag(X%*%H%*%t(X))
    ts <- resid(fit.model,type="pearson")*sqrt(fi/(1-h))
    dim <- (h/(1-h))*(ts^2)
    
    pp = fit.model$dispersion$weights/2
    Z <- matrix(sqrt(pp),n,q)*Z
    R <- solve(t(Z)%*%Z)
    r <- diag(Z%*%R%*%t(Z))
    
    if(fit.model$family$family=="Gamma"){
        t <- log(resp/mu) - resp/mu 
        t1 = t + 1 + log(fi) - digamma(fi)
        tt = t1/sqrt((1-r)*(trigamma(fi) - 1/fi))}
    
    if(fit.model$family$family=="gaussian"){
        t <- resp*mu - 0.5*(mu*mu + resp*resp) 
        t1 = t + 1/(2*fi)
        tt = t1/(sqrt(1-r)*(1/(fi*sqrt(2))))}
    
    if(fit.model$family$family=="inverse.gaussian"){
        t <- -(resp/(2*mu*mu) + 1/mu + 1/(2*mu))
        t1 = t + 1/(2*fi)
        tt = t1/(sqrt(1-r)*(1/(fi*sqrt(2))))}
    
    did = (r/(1-r))*(tt^2)
    
    
    par(mfrow=c(1,2))
    plot(dim, xlab=xlab.mu, ylab=ylab.mu, main=main.mu, pch=pch)
    if(!missingArg(identify.mu)) identify(dim, n=identify.mu)	
    
    plot(did, xlab=xlab.phi, ylab=ylab.phi, main=main.phi, pch=pch)
    if(!missingArg(identify.phi)) identify(did, n=identify.phi)	
}



bc2 <- function(fit.model, rep, conf, xlab.mu, xlab.phi, ylab.mu, ylab.phi, main.mu, main.phi, pch, identify.mu, identify.phi){
    oldw <- getOption("warn")
    options(warn = -1)
    if(class(fit.model)[1] != "dglm") stop("Only dglm-type objects are suported!!",call.=FALSE)
    
    X <- model.matrix(fit.model)
    n <- nrow(X)
    p <- ncol(X)
    Z <- model.matrix(fit.model$dispersion)
    q <- ncol(Z)
    mu <- fitted(fit.model)
    fi <- fitted(fit.model$dispersion)
    fi <- 1/fi
    w <- fit.model$weights
    w <- w*fi
    Xw <- X*matrix(sqrt(w),n,p)
    h <- diag(Xw%*%tcrossprod(solve(crossprod(Xw,Xw)),Xw))
    td <- resid(fit.model,type="deviance")*sqrt(fi/(1-h))
    pp = fit.model$dispersion$weights/2
    Z <- matrix(sqrt(pp),n,q)*Z
    R <- solve(t(Z)%*%Z)
    r <- diag(Z%*%R%*%t(Z))
    tdd <- resid(fit.model$dispersion,type="deviance")/sqrt(1-r)
    
    
    
    
    e <- matrix(0,n,rep)
    ed <- matrix(0,n,rep)
    bar <- txtProgressBar(min=0, max=rep, initial=0, width=min(50,rep), char="+", style=3)
    i <- 1
    while(i<=rep){
        
        if(fit.model$family$family=="Gamma"){
            resp <- rgamma(n,fi)
            resp <- (fitted(fit.model)/fi)*resp
        }
        if(fit.model$family$family=="inverse.gaussian") resp <- rig(n,mu,fi)
        if(fit.model$family$family=="gaussian") resp <- sqrt(1/fi)*rnorm(n,0,1) + mu
        
        dlink=fit.model$dispersion$family$link
        fit <- try(dglm(resp ~ -1 + X, dformula = ~ -1 + Z, family=fit.model$family, dlink=dlink, mustart=mu, phistart=1/fi),silent=TRUE)
        
        if(is.list(fit)){
            ws <- fit$weights
            fis <- fitted(fit$dispersion)
            fis <- 1/fis
            ws <- ws*fis
            Xw <- X*matrix(sqrt(ws),n,p)
            hs <- diag(Xw%*%tcrossprod(solve(crossprod(Xw,Xw)),Xw))
            pp = fit$dispersion$weights/2
            Z <- matrix(sqrt(pp),n,q)*Z
            R <- solve(t(Z)%*%Z)
            r <- diag(Z%*%R%*%t(Z))
            e[,i] <- sort(resid(fit,type="deviance")*sqrt(fis/(1-hs)))
            ed[,i] <- sort(resid(fit$dispersion,type="deviance")/sqrt(1-r))
            setTxtProgressBar(bar,i)
            i <- i + 1
        }
        
        
        
    }
    e1 <- numeric(n)
    e2 <- numeric(n)
    e1d <- numeric(n)
    e2d <- numeric(n)
    alpha <- 1 - conf
    for(i in 1:n){
        eo <- sort(e[i,])
        e1[i] <- quantile(eo,alpha/2)
        e2[i] <- quantile(eo,1-alpha/2)
        eo <- sort(ed[i,])
        e1d[i] <- quantile(eo,alpha/2)
        e2d[i] <- quantile(eo,1-alpha/2)}
    med <- apply(e,1,mean)
    medd <- apply(ed,1,mean)
    
    faixa <- range(td,e1,e2)
    faixad <- range(tdd,e1d,e2d)
    
    if(missingArg(xlab.mu))  xlab.mu="Percentiles de la N(0,1)"
    if(missingArg(ylab.mu))  ylab.mu="Residuo Componente del desvio para localizacion"
    if(missingArg(main.mu))  main.mu=" "
    if(missingArg(xlab.phi))  xlab.phi="Percentiles de la N(0,1)"
    if(missingArg(ylab.phi))  ylab.phi="Residuo Componente del desvio para dispersion"
    if(missingArg(main.phi))  main.phi=" "
    if(missingArg(pch))   pch=16
    
    close(bar)
    
    par(mfrow=c(1,2))
    outm <- qqnorm(td, xlab="", ylab="", ylim=faixa, main="", pch=pch)
    par(new=TRUE)
    qqnorm(e1,axes=FALSE,xlab="",ylab="",main="", type="l",ylim=faixa,lty=1)
    par(new=TRUE)
    qqnorm(e2,axes=FALSE,xlab="",ylab="", main="", type="l",ylim=faixa,lty=1)
    par(new=TRUE)
    qqnorm(med,axes=FALSE,xlab=xlab.mu,ylab=ylab.mu,main=main.mu,type="l",ylim=faixa,lty=2)
    if(!missingArg(identify.mu)) identify(outm$x,outm$y, n=identify.mu)
    
    outm <- qqnorm(tdd, xlab="", ylab="", ylim=faixad, main="", pch=pch)
    par(new=TRUE)
    qqnorm(e1d,axes=FALSE,xlab="",ylab="",main="", type="l",ylim=faixad,lty=1)
    par(new=TRUE)
    qqnorm(e2d,axes=FALSE,xlab="",ylab="", main="", type="l",ylim=faixad,lty=1)
    par(new=TRUE)
    qqnorm(medd,axes=FALSE,xlab=xlab.phi,ylab=ylab.phi,main=main.phi,type="l",ylim=faixad,lty=2)
    if(!missingArg(identify.phi)) identify(outm$x,outm$y, n=identify.phi)
    options(warn = oldw)
}
#########################################################################################################


##########################
#Deteccion de datos influyentes y atipicidad
##########################


influenceIndexPlot(modelo2,main = "Primer Analisis")
gl=modelo2$df.residual
plot(cooks.distance(modelo2),
     residuals(modelo2, type="deviance"),
     main="D Cook vs Dev",pch=19,
     xlab="Distancia de cook",
     ylab="Residuales de deviance",
     ylim=c(-3,3),cex=0.6)
abline(h=2.5,col='red')
abline(h=-2.5,col='red')
abline(v=(2*7)/(gl),col='blue')

###### CORRECCION DE DATOS ATIPICOS e influyentes
##########################3
# Segundo analisis
#########################
modelo4<-glm(QN25 ~ Q2 + QN20 + QN22 + QN23 + QN35 + QN44 + 
                 QN53 + QN56 + Q2:QN20 + QN20:QN44 + QN22:QN44 + QN20:QN53 + 
                 QN23:QN53 + QN35:QN53 + QN22:QN23,data=Datos3
             [(row.names(Datos3)!="525")&(row.names(Datos3)!="1179")&(row.names(Datos3)!="1334")
                 &(row.names(Datos3)!="1519 ")&(row.names(Datos3)!="2858")&(row.names(Datos3)!="2812")
                 &(row.names(Datos3)!="3088") &(row.names(Datos3)!="1878")&(row.names(Datos3)!="2816")
                 &(row.names(Datos3)!="464 ")&(row.names(Datos3)!="632")&(row.names(Datos3)!="1519"),],binomial)
summary(modelo4) #1466.6
#influencia
influenceIndexPlot(modelo4,main = "Segundo Analisis")
gl=modelo4$df.residual
plot(cooks.distance(modelo4),
     residuals(modelo4, type="deviance"),
     main="D Cook vs Dev",pch=19,
     xlab="Distancia de cook",
     ylab="Residuales de deviance",
     ylim=c(-3,3),cex=0.6)
abline(h=2.5,col='red')
abline(h=-2.5,col='red')
abline(v=(2*8)/(gl),col='blue')
##########################################################################
# Tercer analisis
##########################################################################
modelo5<-glm(QN25 ~ Q2 + QN20 + QN22 + QN23 + QN35 + QN44 + 
                     QN53 + QN56 + Q2:QN20 + QN20:QN44 + QN22:QN44 + QN20:QN53 + 
                     QN23:QN53 + QN35:QN53 + QN22:QN23,data=Datos3
                 [(row.names(Datos3)!="525")&(row.names(Datos3)!="1179")&(row.names(Datos3)!="1334")
                     &(row.names(Datos3)!="1519 ")&(row.names(Datos3)!="2858")&(row.names(Datos3)!="2812")
                     &(row.names(Datos3)!="3088") &(row.names(Datos3)!="1878")&(row.names(Datos3)!="2816")
                     &(row.names(Datos3)!="464")&(row.names(Datos3)!="632")&(row.names(Datos3)!="1519")
                     &(row.names(Datos3)!="1766") &(row.names(Datos3)!="2278")&(row.names(Datos3)!="3264")
                     &(row.names(Datos3)!="172")&(row.names(Datos3)!="464")&(row.names(Datos3)!="922")
                     &(row.names(Datos3)!="1161") 
                     ,],binomial)
summary(modelo5) #1435.5
#influencia
influenceIndexPlot(modelo5,main = "Tercer Analisis")
gl3=modelo5$df.residual
plot(cooks.distance(modelo5),
     residuals(modelo5, type="deviance"),
     main="D Cook vs Dev",pch=19,
     xlab="Distancia de cook",
     ylab="Residuales de deviance",
     ylim=c(-3,3),cex=0.6)
abline(h=2.5,col='red')
abline(h=-2.5,col='red')
abline(v=(2*8)/(gl3),col='blue')
llista13<-Datos3[which( abs(rstudent(modelo5))> 2.5), ];llista13
llista23<-Datos3[which( abs(cooks.distance(modelo5))> (2*8)/(gl)), ];llista23
###########################################
#Mejor enlace
##########################################
modelo5<-glm(QN25 ~ Q2 + QN20 + QN22 + QN23 + QN35 + QN44 + 
                 QN53 + QN56 + Q2:QN20 + QN20:QN44 + QN22:QN44 + QN20:QN53 + 
                 QN23:QN53 + QN35:QN53 + QN22:QN23,data=Datos3
             [(row.names(Datos3)!="525")&(row.names(Datos3)!="1179")&(row.names(Datos3)!="1334")
                 &(row.names(Datos3)!="1519 ")&(row.names(Datos3)!="2858")&(row.names(Datos3)!="2812")
                 &(row.names(Datos3)!="3088") &(row.names(Datos3)!="1878")&(row.names(Datos3)!="2816")
                 &(row.names(Datos3)!="464")&(row.names(Datos3)!="632")&(row.names(Datos3)!="1519")
                 &(row.names(Datos3)!="1766") &(row.names(Datos3)!="2278")&(row.names(Datos3)!="3264")
                 &(row.names(Datos3)!="172")&(row.names(Datos3)!="464")&(row.names(Datos3)!="922")
                 &(row.names(Datos3)!="1161") 
                 ,],family=binomial("logit"))

# Enlace probit
modelo5_pro<-glm(QN25 ~ Q2 + QN20 + QN22 + QN23 + QN35 + QN44 + 
                     QN53 + QN56 + Q2:QN20 + QN20:QN44 + QN22:QN44 + QN20:QN53 + 
                     QN23:QN53 + QN35:QN53 + QN22:QN23,data=Datos3
                 [(row.names(Datos3)!="525")&(row.names(Datos3)!="1179")&(row.names(Datos3)!="1334")
                     &(row.names(Datos3)!="1519 ")&(row.names(Datos3)!="2858")&(row.names(Datos3)!="2812")
                     &(row.names(Datos3)!="3088") &(row.names(Datos3)!="1878")&(row.names(Datos3)!="2816")
                     &(row.names(Datos3)!="464 ")&(row.names(Datos3)!="632")&(row.names(Datos3)!="1519")
                     &(row.names(Datos3)!="1766") &(row.names(Datos3)!="2278")&(row.names(Datos3)!="3264")
                     &(row.names(Datos3)!="172")&(row.names(Datos3)!="464")&(row.names(Datos3)!="922")
                     &(row.names(Datos3)!="1161") 
                     ,],binomial(link="probit"))

#### CAUCHIT 
modelo5_cauchit<-glm(QN25 ~ Q2 + QN20 + QN22 + QN23 + QN35 + QN44 + 
                         QN53 + QN56 + Q2:QN20 + QN20:QN44 + QN22:QN44 + QN20:QN53 + 
                         QN23:QN53 + QN35:QN53 + QN22:QN23,data=Datos3
                     [(row.names(Datos3)!="525")&(row.names(Datos3)!="1179")&(row.names(Datos3)!="1334")
                         &(row.names(Datos3)!="1519 ")&(row.names(Datos3)!="2858")&(row.names(Datos3)!="2812")
                         &(row.names(Datos3)!="3088") &(row.names(Datos3)!="1878")&(row.names(Datos3)!="2816")
                         &(row.names(Datos3)!="464 ")&(row.names(Datos3)!="632")&(row.names(Datos3)!="1519")
                         &(row.names(Datos3)!="1766") &(row.names(Datos3)!="2278")&(row.names(Datos3)!="3264")
                         &(row.names(Datos3)!="172")&(row.names(Datos3)!="464")&(row.names(Datos3)!="922")
                         &(row.names(Datos3)!="1161") 
                         ,],binomial(link="cauchit")) #AIC=167.22

AIC(modelo5,modelo5_pro,modelo5_cauchit)


##########################################################################
# VALIDACION DE SUPUESTOS Modelo logit
##########################################################################

# Autocorrelaccion orden 1
set.seed(51428787)
library(lmtest)
dwtest(modelo5)
# Funccion de Autocorrelacci?n
cr <- acf(rstudent(modelo5), lag =  20)

# Distribucion normal asintotica

envelope_glm(modelo5) # distribuye asintoticamente normal
# Anaisis de multicolinealidad1
vif_glm(modelo5) #no hay multicolinealidad

# residuales
residuals_glm(modelo5,main="Primer nivel") # esta bien no pasa de 3 o -3
# Medidas de bondad de ajuste
GOF_glm(modelo5) # se compara con otros modelos, los de menor AIC se escoge

# Deteccion de datos influyentes POSTERIORI
Cookdis_glm(modelo5,main="Influencia a posteriori",
            ylab="Distancia de Cook",
            xlab="Observaci?n",
            col="blue4")


#### logit ####
E1<- residuals(modelo5, type="pearson")
F1<- predict(modelo5, type="response")
plot(E1)
plot(G1)
## Residuales
G1<- residuals(modelo5, type="deviance") ## residuales devianza (clasico)
plot(G1, main="Residuales Deviance")
residuals_glm(modelo5)
#Relacion entre variables y residuales


d=Datos3[(row.names(Datos3)!="525")&(row.names(Datos3)!="1179")&(row.names(Datos3)!="1334")
    &(row.names(Datos3)!="1519 ")&(row.names(Datos3)!="2858")&(row.names(Datos3)!="2812")
    &(row.names(Datos3)!="3088") &(row.names(Datos3)!="1878")&(row.names(Datos3)!="2816")
    &(row.names(Datos3)!="464 ")&(row.names(Datos3)!="632")&(row.names(Datos3)!="1519")
    &(row.names(Datos3)!="1766") &(row.names(Datos3)!="2278")&(row.names(Datos3)!="3264")
    &(row.names(Datos3)!="3267")&(row.names(Datos3)!="3076")&(row.names(Datos3)!="2266")
    &(row.names(Datos3)!="2167")
    ,]
Q2_3 = d$Q2
#length(G1)
#length(Q2_3)
QN20_1 = d$QN20
QN22_1 = d$QN22
QN23_1 = d$QN23
QN35_1 = d$QN35
QN44_1 = d$QN44
QN53_1 = d$QN53
QN56_1 = d$QN56

plot(Q2_3,G1, xlab="sexo", ylab="Pearson")
abline(h=0,col=2)
plot(QN20_1,G1,xlab="Bullyng", ylab="Pearson")
abline(h=0,col=2)
plot(QN22_1,G1,xlab="Soledad", ylab="Pearson")
abline(h=0,col=2)
plot(QN23_1,G1,xlab="preocupacion", ylab="Pearson")
abline(h=0,col=2)
plot(QN44_1,G1,xlab="Ha tenido sexo", ylab="Pearson")
abline(h=0,col=2)
plot(QN35_1,G1,xlab="Bebido alcohol", ylab="Pearson")
abline(h=0,col=2)
plot(QN53_1,G1,xlab="faltado a la escuela", ylab="Pearson")
abline(h=0,col=2)
plot(QN56_1,G1,xlab="Preocupacion de los padres", ylab="Pearson")
abline(h=0,col=2)

###Curva ROC#####
testdata = predict.fit
#Con el modelo final
library(Epi)
ROC(form=QN25 ~ Q2 + QN20 + QN22 + QN23 + QN35 + QN44 + 
        QN53 + QN56 + Q2:QN20 + QN20:QN44 + QN22:QN44 + QN20:QN53 + 
        QN23:QN53 + QN35:QN53 + QN22:QN23,data=Datos3
    [(row.names(Datos3)!="525")&(row.names(Datos3)!="1179")&(row.names(Datos3)!="1334")
        &(row.names(Datos3)!="1519 ")&(row.names(Datos3)!="2858")&(row.names(Datos3)!="2812")
        &(row.names(Datos3)!="3088") &(row.names(Datos3)!="1878")&(row.names(Datos3)!="2816")
        &(row.names(Datos3)!="464 ")&(row.names(Datos3)!="632")&(row.names(Datos3)!="1519")
        &(row.names(Datos3)!="1766") &(row.names(Datos3)!="2278")&(row.names(Datos3)!="3264")
        &(row.names(Datos3)!="172")&(row.names(Datos3)!="464")&(row.names(Datos3)!="922")
        &(row.names(Datos3)!="1161"),])

####################################################################################
