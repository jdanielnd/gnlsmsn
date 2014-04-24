#' Fit univariate NL-SMSN regression
#'
#' This function is used to fit univariate NL-SMSN regression using a GUI
#'
#' @importFrom nlsmsn smsn.nl
#' @export
gsmsn.nl <- function() {

  .env <- environment()
  model <- NULL

  # Top frame 
  tt <- tktoplevel()
  tkwm.title(tt,"smsn.nl")

  # Read data file
  frame0 <- ttklabelframe(tt, text = "Data")
    tkgrid(frame0)
    nomearquivo1 <- tclVar("")
    aux<- tclVar(0)
    entry.nomearquivo<-tkentry(frame0,width="20",textvariable=nomearquivo1) 
    ler <-function() {
      tclvalue(aux) <- 1
      tclvalue(nomearquivo1) <- tkgetOpenFile()
    }
    ok1<-function() {
      tkdestroy(frame0)
    }
    ler.but <- tkbutton(frame0,text="...",command= ler)
    ok.but1 <- tkbutton(frame0,text="Load",command= ok1)
    tkgrid(entry.nomearquivo, ler.but)
    tkgrid(ok.but1)
    tkbind(tt,"<Destroy>", function() tclvalue(aux) <- 2) 
    tkwait.variable(aux)
    dados<-NULL
    if(tclvalue(aux)== 1){
      dados=read.table(tclvalue(nomearquivo1), header = T)
    }

  tkwait.window(frame0)

  frameTop <- tkframe(tt)
  frameA <- tkframe(frameTop, borderwidth=2, relief="groove")
  frameB<-tkframe(frameTop, relief = "groove", borderwidth = 2)
  frameC <- tkframe(frameTop, relief = "groove", borderwidth = 2)
  tkgrid(frameA, frameB, frameC)

  #-------------- Mistura

  frame1 <- ttklabelframe(frameA, text = "Mixture")
    rbValue <- tclVar("t")
    rb1 <- tkradiobutton(frame1,variable=rbValue)
    rb2 <- tkradiobutton(frame1,variable=rbValue)
    rb3 <- tkradiobutton(frame1,variable=rbValue)
    rb4 <- tkradiobutton(frame1,variable=rbValue)
    rb5 <- tkradiobutton(frame1,variable=rbValue)
    rb6 <- tkradiobutton(frame1,variable=rbValue)


  tkgrid(tklabel(frame1,text="t"),rb1)
  tkgrid(tklabel(frame1,text="Skew.t"),rb2)
  tkgrid(tklabel(frame1,text="Skew.cn"),rb3)
  tkgrid(tklabel(frame1,text="Skew.slash"),rb4)
  tkgrid(tklabel(frame1,text="Skew.normal"),rb5)
  tkgrid(tklabel(frame1,text="Normal"),rb6)
  tkgrid(frame1)

  #---------------- Variavel Resposta


  frame2<- ttklabelframe(frameA, text="Response variable") 
    combo.names1 = c("",names(dados))
    comboBoxVar1 <- tclVar() 
    tclvalue(comboBoxVar1) <- combo.names1[1] 
    comboBox01 <- ttkcombobox(frame2, values=combo.names1, 
    textvariable=comboBoxVar1,width="10", state="readonly") 
  tkgrid(comboBox01)
  tkgrid(frame2)


  #-----
  frame3<- ttklabelframe(frameA, text="Covariates") 
  cb<-list()
  cbvalue<-list()

  for(i in 1:length(names(dados))){
    cb[[i]]<- tkcheckbutton(frame3)
    cbvalue[[i]]<- tclVar(0)
    tkconfigure(cb[[i]],variable=cbvalue[[i]],state="normal")
    tkgrid(tklabel(frame3,text=names(dados)[i]),cb[[i]])
  }

  tkgrid(frame3)


  #tt <- tktoplevel()
  #tkwm.title(tt,"Prova")

  #--------------Chutes Iniciais
  frame4 <- ttklabelframe(frameC, text = "Initial values") 

  #------Sigma2
    sigma2 <- tclVar("0") 
    entry.sigma2 <-tkentry(frame4,width="12",textvariable=sigma2) 
    tkgrid(tklabel(frame4,text="sigma2"),entry.sigma2) 

  #------shape
    shape <- tclVar("0") 
    entry.shape <-tkentry(frame4,width="12",textvariable=shape) 
    tkgrid(tklabel(frame4,text="shape"),entry.shape) 

  #------rho
    rho <- tclVar("0") 
    entry.rho <-tkentry(frame4,width="12",textvariable=rho,state="disabled") 
    tkgrid(tklabel(frame4,text="rho"),entry.rho) 
  tkgrid(frame4) 

  #------nu
    nu <- tclVar("0") 
    entry.nu <- tkentry(frame4, width="12", textvariable=nu) 
    tkgrid(tklabel(frame4,text="nu"), entry.nu) 
  tkgrid(frame4) 

    disable_nu <- function() {
      tkconfigure(entry.nu, state="disabled")
    }
    enable_nu <- function() {
      tkconfigure(entry.nu, state="normal")
    }
    tkconfigure(rb1,value="t",width=4, command=enable_nu)
    tkconfigure(rb2,value="Skew.t", command=enable_nu)
    tkconfigure(rb3,value="Skew.cn", command=enable_nu)
    tkconfigure(rb4,value="Skew.slash", command=enable_nu)
    tkconfigure(rb5,value="Skew.normal",command=disable_nu)
    tkconfigure(rb6,value="Normal",command=disable_nu)

  #------betas
    betas <- tclVar("c(1,1,1)") 
    entry.betas <-tkentry(frame4,width="12",textvariable=betas) 
    tkgrid(tklabel(frame4,text="betas"),entry.betas)
  tkgrid(frame4) 

  # Regression type

  regtype.frame<-tkframe(frameB,relief="groove",borderwidth=2)

  z.reg <- c("Homoscedastic","Heteroscedastic")
  reg.type <- tclVar(z.reg[1])

  combobox.reg<-ttkcombobox(regtype.frame,values=z.reg,textvariable=reg.type,state="readonly")

  tkgrid(tklabel(regtype.frame,text="Regression type:"))
  tkgrid(combobox.reg)
  tkgrid(regtype.frame)

  # Rho function

  rho.frame<-tkframe(frameB,relief="groove",borderwidth=2)

  z.rho <- c("exp(z*rho)","z^rho")
  rho.func <- tclVar(z.rho[1])

  combobox.rho<-ttkcombobox(rho.frame,values=z.rho,textvariable=rho.func,state="disabled")

  tkgrid(tklabel(rho.frame,text="Rho function:"))
  tkgrid(combobox.rho)
  tkgrid(rho.frame)

  heteroscedastic <- function() {
    tclvalue(reg.type)==z.reg[2]
  }

  normal <- function() {
    tclvalue(rbValue) == "Normal" || tclvalue(rbValue) == "Skew.normal"
  }


  # z variables

  z.frame<-tkframe(frameB, relief = "groove", borderwidth = 2)
  tkgrid(tklabel(z.frame, text = "Z variables:"), columnspan = 2)

  cbuttons <- list()
  cbValues <- list()
  variables <- names(dados)
  for(i in 1:length(variables)) {
    cbuttons[[i]] <- tkcheckbutton(z.frame)
    cbValues[[i]] <- tclVar(0)
    tkconfigure(cbuttons[[i]],variable = cbValues[[i]], state="disabled")
    tkgrid(tklabel(z.frame,text = variables[i]), cbuttons[[i]])
  }
  tkgrid.configure(z.frame, sticky = "we")
  tkgrid(z.frame)

  func1<-function() {
    j = which(names(dados)==tclvalue(comboBoxVar1))
    tkconfigure(cb[[j]],state="disabled")
    tkconfigure(cbuttons[[j]],state="disabled")
    o = (1:length(names(dados)))[-j]
    for (i in o){
      tkconfigure(cb[[i]],state="normal")
      if(heteroscedastic()) tkconfigure(cbuttons[[i]],state="normal")
    }
  }

  tkbind(comboBox01, "<<ComboboxSelected>>", func1)

  checkType <- function() {
    if(heteroscedastic()) {
      tkconfigure(combobox.rho,state="readonly")
      for(i in 1:length(variables)) {
        if(names(dados)[i]!=tclvalue(comboBoxVar1)) tkconfigure(cbuttons[[i]], state="normal")
      }
      tkconfigure(entry.rho, state="normal")
    }
    if(!heteroscedastic()) {
      tkconfigure(combobox.rho,state="disabled")
      for(i in 1:length(variables)) {
        tkconfigure(cbuttons[[i]], state="disabled")
      }
      tkconfigure(entry.rho, state="disabled")
    }
  }

  tkbind(combobox.reg, "<<ComboboxSelected>>", checkType)

  # Linear function

  tkgrid(tklabel(frameC,text="Linear function:"))

  txt <- tktext(frameC, width = 30, height = 8)
  tkgrid(txt)
  tkmark.set(txt,"insert","0.0")

  frameD <- tkframe(tt, borderwidth=2, relief="groove")

  criteriabutton <- tkcheckbutton(frameD)
  criteriaValue <- tclVar(0)
  tkconfigure(criteriabutton, variable = criteriaValue)

  Ok <-function() {
    code <- tclvalue(tkget(txt,"0.0","end"))
    assign("linear.func", eval(parse(text=code)))
    covar <- cbind(variables,unlist(lapply(cbvalue, tclvalue)))
    covar.names <- covar[covar[,2]=="1",1]
    covar.var <- cbind(variables,unlist(lapply(cbValues, tclvalue)))
    covar.var.names <- covar.var[covar.var[,2]=="1",1]
    var <- tclvalue(comboBoxVar1)
    nu_value <- as.numeric(tclvalue(nu))
    if(normal()) nu_value <- NULL
    model <- smsn.nl(
      y = dados[,var],
      x = dados[,covar.names],
      z = ifelse(heteroscedastic(), dados[,covar.var.names], NULL),
      betas = eval(parse(text=tclvalue(betas))),
      sigma2 = as.numeric(tclvalue(sigma2)),
      shape = as.numeric(tclvalue(shape)),
      rho = ifelse(heteroscedastic(), as.numeric(tclvalue(rho)), NULL),
      nu = nu_value,
      nlf = linear.func,
      reg.type = tclvalue(reg.type),
      criteria = as.logical(as.numeric(tclvalue(criteriaValue))),
      family = tclvalue(rbValue),
      iter.max = 200
    )
    assign("model", model, envir = .env)
    tkdestroy(tt)
  }
  ok.but <- tkbutton(frameD,text=" Ok ",command= Ok)
  tkgrid(tklabel(frameD,text = "Comparison criteria"), criteriabutton)
  tkgrid(ok.but)
  tkgrid(frameTop)
  tkgrid(frameD)
  tkwait.window(tt)
  model
}