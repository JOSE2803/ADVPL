#INCLUDE "Totvs.ch"
#INCLUDE "Protheus.ch"   
#INCLUDE "RwMake.ch"
#INCLUDE "TbiConn.CH"

User Function CrtPedCp()
        
    	Private aTxt	:= {} //Array que irá receber as informações do TXT.
    	Private cCodFnc	:= Space(6)
    	Private cCndPag := Space(3)
    	Private cContat := Space(15)
    	Private cArquiv	:= Space(100)
    	Private oCodFnc, oCndPag, oContat, oArquiv
   	Private oDlg
   		
   	DlgHome()
    	
Return

//A função LerTxt() lê o arquivo TXT contido em algum diretório e passa as informações para um array.
Static Function LerTxt() 

	Local cLinha	:= ""
	Local lRet	:= .F.
	
	CursorWait()
	
	aTxt := {}
			
	If !File(cArquiv) //Verifica se o caminho informado do arquivo TXT é valido.
	
		Aviso("Arquivo","Arquivo não selecionado ou invalido.",{"Sair"},1)
        	Return
		
    	Else
	
		FT_FUse(cArquiv)
	
		FT_FGoTop()
	
		While !FT_FEOF()
		
			cLinha := FT_FReadLn()
			cLinha := AllTrim(cLinha)
			
			//Passa para o array aTxt as informações do TXT.
			//Nesse caso ele considera que cada coluna do TXT é separada por ";".
			//A função Separa() faz essa separação.
			aAdd(aTxt,	Separa(cLinha, ";", .F.))
	
			FT_FSkip()
	
		EndDo
	
		FT_FUse()
		
		ExcAutPc()
		
	EndIf
	
	CursorArrow()

Return

//Função ExcAutPc() realiza o processo do ExecAuto.
Static Function ExcAutPc()
    
	Local nI	:= 0
	Local aCmpCab	:= {}
	Local aCmpLst	:= {}
	Local aLstPrd	:= {}
	Local cNumPdc	:= GetNumSC7()                                                                                                                     
	Local dDtaEmi	:= dDataBase
	Local cCodLoj	:= "01"
	Local cFilEnt	:= cFilAnt
	Local cCodPrd	:= ""
	Local nQtdPrd	:= 0
	Local nPrcUnt	:= 0
	
	Private lMsErroAuto := .F.
	
	CursorWait()
	
	Begin Transaction   	
	
		aCmpCab := {}
		aCmpLst := {}
	    
		aAdd(aCmpCab,{"C7_NUM",		cNumPdc,})
		aAdd(aCmpCab,{"C7_EMISSAO",	dDtaEmi,})	
		aAdd(aCmpCab,{"C7_FORNECE",	cCodFnc,})	
		aAdd(aCmpCab,{"C7_LOJA",	cCodLoj,})			
		aAdd(aCmpCab,{"C7_COND",	cCndPag,})	
		aAdd(aCmpCab,{"C7_CONTATO",	cContat,})	
		aAdd(aCmpCab,{"C7_FILENT",	cFilEnt,})
		
		For nI := 1 To Len(aTxt)
		
			cCodPrd := aTxt[nI, 1]
			nQtdPrd := Val(aTxt[nI, 2])
			nPrcUnt := Val(aTxt[nI, 3])
			
			aCmpLst := {}
		
			aAdd(aCmpLst,{"C7_PRODUTO",	cCodPrd,})	
			aAdd(aCmpLst,{"C7_QUANT",	nQtdPrd,})	
			aAdd(aCmpLst,{"C7_PRECO",	nPrcUnt,}) 
			
			aAdd(aLstPrd, aCmpLst)			
		
		Next nI
				
		MATA120(1, aCmpCab, aLstPrd, 3)
		
		If lMsErroAuto
			MostraErro()
	        	DisarmTransaction()
	    	Else
			MsgInfo("Pedido de compra incluído com sucesso!")
			MsgInfo("Pedido número: " + cNumPdc)		
			cCodFnc	:= ""
			cCodFnc := Space(6)
			cCndPag := ""
			cCndPag := Space(3)
			cContat := "" 
			cContat := Space(15)
			cArquiv	:= ""  
			cArquiv := Space(100)
			oCodFnc:SetFocus()
		EndIf
		
	End Transaction
	
	CursorArrow()

Return

//Função DlgHome que monta uma janela com botões e comandos para facilidade do usuário.
Static Function DlgHome()

	Local oBtn1, oBtn2
	Local oSay
	Local oFont1
	
	DEFINE FONT oFont1 BOLD SIZE 0,30
		
	DEFINE DIALOG oDlg TITLE "Inclusão de Pedido de Compra via arquivo TXT" FROM 0,0 TO 200,800 COLOR CLR_BLACK,CLR_WHITE PIXEL
	
	@ 001,001 TO 20,399 OF oDlg PIXEL
	
	@ 005,000 SAY oSay PROMPT "FILIAL ATIVA - " + AllTrim(cValToChar(cFilAnt)) SIZE 400,10 OF oDlg CENTER PIXEL FONT oFont1 COLOR CLR_RED
	
	@ 020,001 TO 080,399 OF oDlg PIXEL

	@ 030,005 SAY oSay PROMPT "CÓD. DO FORNECEDOR:" SIZE 60,12 OF oDlg PIXEL
	@ 045,005 SAY oSay PROMPT "COND. PAGAMENTO:" SIZE 60,12 OF oDlg PIXEL
	@ 060,005 SAY oSay PROMPT "CONTATO:" SIZE 60,12 OF oDlg PIXEL
	@ 030,160 SAY oSay PROMPT "OBSERVAÇÃO: O arquivo TXT deve conter o código do produto, ";
	+ "a quantidade, o preço unitário, nesta mesma ordem e cada campo separado por ponto e vírgula ";
	+ "(;), caso contrário a importação do TXT para o pedido de compra não será concluída!" SIZE 220,36 OF oDlg PIXEL
	@ 060,160 SAY oSay PROMPT "ARQUIVO TXT:" SIZE 60,12 OF oDlg PIXEL
	
	@ 029,070 MSGET oCodFnc VAR cCodFnc VALID ExistCpo("SA2", cCodFnc, 1) F3 "SA2" SIZE 20, 10 OF oDlg PIXEL HASBUTTON
	@ 044,070 MSGET oCndPag VAR cCndPag VALID ExistCpo("SE4", cCndPag, 1) F3 "SE4" SIZE 20, 10 OF oDlg PIXEL HASBUTTON
	@ 059,070 MSGET oContat VAR cContat SIZE 80, 10 OF oDlg PIXEL HASBUTTON
	@ 059,200 MSGET oArquiv VAR cArquiv SIZE 170, 10 OF oDlg PIXEL HASBUTTON
	
	@ 080,001 TO 0100,399 OF oDlg PIXEL
	 
	@ 084,005 BUTTON oBtn1 PROMPT "SALVAR" SIZE 40, 013 OF oDlg ACTION LerTxt() PIXEL
	@ 058,370 BUTTON oBtn2 PROMPT "..." SIZE 20, 013 OF oDlg ACTION cArquiv := cGetFile("Arquivo TXT *.txt |*.TXT", "Arquivo TXT")  PIXEL
	ACTIVATE DIALOG oDlg CENTER

Return
