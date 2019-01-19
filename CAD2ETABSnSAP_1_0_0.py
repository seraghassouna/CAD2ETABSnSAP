#Author: Serag Hassouna
"""
This module contains of:-
1- A GUI to interact with user
2- Functions that import model to ETABS 2016
3- Functions that import model to SAP2000 v18
"""

#Import needed modules
import Tkinter as tk
import ttk
import Pmw
#from Tkinter import *
#from ttk import Frame, Button, Style, Notebook
from ttk import *
from tkFileDialog import askopenfilename
from tkMessageBox import showinfo, showerror, INFO, ERROR

import sys
import os
import comtypes.client
from comtypes import COMError
from comtypes.client import CreateObject, GetModule, GetActiveObject

import array

#test part

#load all needed type libraries

'''
GetModule("C:/Windows/System32/stdole2.tlb")
import comtypes.gen.stdole as ole
print "stdole2 successfully loaded"
GetModule("C:/Program Files/Common Files/Autodesk Shared/acax20enu.tlb")
import comtypes.gen._4E3F492A_FB57_4439_9BF0_1567ED84A3A9_0_1_0 as acax
print "acax20enu successfully loaded"
GetModule("C:/Program Files/Common Files/Autodesk Shared/AcSmComponents20.tlb")
import comtypes.gen._ED125AFF_6294_4BE4_81E2_B98DCBBA214E_0_1_0 as AcSm
print "AcSmComponents20 successfully loaded"
GetModule("C:/Program Files/Computers and Structures/ETABS 2016/ETABS2016.tlb")
import comtypes.gen.ETABS2016 as E2016
print "ETABS2016 logic successfully loaded"
'''

#GetModule("C:\\Program Files\\Common Files\\Autodesk Shared\\cao20enu.tlb") #can't load
#import comtypes.gen.cao20enu
#print "cao20enu successfuly loaded"
#GetModule("C:\\Program Files\\Common Files\\Autodesk Shared\\axdbenu.tlb") #can't load
#import comtypes.gen.axdbenu
#print "axdbenu successfuly loaded"

'''
GetModule("C:\Program Files\Common Files\Autodesk Shared\AcETransmit19.tlb")
#import comtypes.gen._2303C1B4_CF88_4A05_B7CC_9C73A1AEFB3E_0_19_0
import comtypes.gen.TRANSMITTALLib as AcETrans
print "AcETransmit19 successfully loaded"
'''
try:
    acad = CreateObject("AutoCAD.Application.20",dynamic=True)
except:
    showerror(title="CAD2ETABSnSAP 1.0.0",message="Can't load AutoCAD, reboot windows and try again")
    sys.exit(-1)
print "AutoCAD successfully loaded"
xlpath = os.getcwd()
xlpath += '\\facilitate.xlsm'
xl = CreateObject("Excel.Application")
from comtypes.gen.Excel import xlRangeValueDefault
xlwb = xl.Workbooks.Open(Filename=xlpath,ReadOnly=0)
print "Facilitator workbook successfully loaded"

progname = "CAD2ETABSnSAP 1.0.0" #Program's Name

#The GUI
class MYWINDOW(Frame):
    def __init__(self,master=None):
        Frame.__init__(self,master) #run the Parent's initiation

        #<<Every variable defined in any method and starts with "self." is considered global on the scope of its class,
        #Otherwise, it's considered local to this method>>
        
        #Sadly, initialize some "global mechanism" variables [policy is not separated from mechanism at Class's Scope for AutoCAD only]
        self.filepath =  tk.StringVar(value="") #the autocad's document file path
        self.oldfilepath = "" #the file name of the previously opened document, that needs to be closed to let the space to the newly requested one
        try:
            self.acad = acad #the AutoCAD instance
        except:
            self.master.destroy
            sys.exit(-1)
        self.xl = xl #the Excel instance
        self.xlwb = xlwb #the specific needed workbook
        self.acadoc = None #Initialize the autocad's document
        self.modelspace = None #initialize the autocad file's model space
        
        #construct the UI
        self.InitUI()
        self.centerwin()

    def InitUI(self):
        #Assign title and style
        self.master.title(progname)
        self.style = Style()
        #self.style.theme_create("custom_cmbox",parent="alt",settings={
        #    "TCombobox": {
        #        "configure": {"background": 'white'}
        #        }
        #    })
        self.style.theme_use("winnative")
        self.master.wm_iconbitmap('icon_csssol.ico')
        for row in range(6):
            self.grid_rowconfigure(row,minsize=25)
        for col in range(6):
            self.grid_columnconfigure(col,minsize=10)
        #make the parent window appears
        self.grid()
        #self.pack(fill=tk.BOTH,expand=True)
        
        #make 2 tabs: SAP2000, ETABS
        n=Notebook(self.master)
        n.grid(row=0,column=0)
        #n.pack(side=TOP,fill=BOTH,expand=True,anchor=NW)
        #n.place(x=0,y=0,relheight=1,relwidth=1)

        #Construct tab ETABS
        fetabs = Frame(n)
        tetabs= n.add(fetabs,text='ETABS')
        #n.tab(fetabs).focus_set()
        gtetabs=Pmw.Group(fetabs,tag_text='ETABS Import Options')
        gfetabs = gtetabs.interior() #the frame of the labeled group
        gtetabs.grid(row=1,column=1,padx=5,pady=5)
        
        impetabs=tk.Button(fetabs,text="Import ETABS Model",width=20,relief=tk.RAISED,command=self.Imp_Etabs)
        impetabs.grid(row=3,column=1,padx=5,pady=5,sticky=tk.N+tk.E+tk.S+tk.W)

        etabs_swmlbl=tk.Label(gfetabs,text='DEAD Self Weight Multiplier')
        etabs_swmlbl.grid(row=1,column=1,padx=5,pady=5)
        self.swmetabs=tk.StringVar(value='1')
        etabs_swmcmbox=ttk.Combobox(gfetabs,state='readonly',values=['0','1'],textvariable=self.swmetabs,width=5) #dead load self weight modifier
        etabs_swmcmbox.grid(row=1,column=2,padx=5,pady=5)

        etabs_elemdlbl = tk.Label(gfetabs,text='Elements Section Property Modifiers')
        etabs_elemdlbl.grid(row=2,column=1,padx=5,pady=5)
        self.elemdetabs=tk.StringVar(value='All set to 1')
        etabs_elemdcmbox=ttk.Combobox(gfetabs,state='readonly',textvariable=self.elemdetabs,values=['All set to 1','As Per ACI M318 11','Torsional Modifiers Only','Egyptian Standard'],width=25)
        etabs_elemdcmbox.grid(row=2,column=2,padx=5,pady=5)

        gtacadetabs=Pmw.Group(fetabs,tag_text='AutoCAD Import Options')
        gfacadetabs=gtacadetabs.interior() #get the frame of the labeled group
        gtacadetabs.grid(row=1,column=2,padx=5,pady=5,sticky=tk.N)

        acadetabscollbl=tk.Label(gfacadetabs,text='Columns Layer') #Note that Columns layers are subject to update, so every updateable combobox must start with "self" to expose it within the class's scope
        acadetabscollbl.grid(row=1,column=1,padx=5,pady=5)
        self.acadetabscollyr=tk.StringVar(value='0')
        self.acadetabscolcmbox=ttk.Combobox(gfacadetabs,state='readonly',values=['None','0'],textvariable=self.acadetabscollyr,postcommand=self.updLayers)
        self.acadetabscolcmbox.grid(row=1,column=2,padx=5,pady=5)

        #add wall crack mode options
        getabswall=Pmw.Group(gfetabs,tag_text='Wall crack mode',tagindent=2)
        gfetabswall=getabswall.interior() #the frame of the labled group
        getabswall.grid(row=1,column=3,padx=5,pady=5,sticky=tk.N+tk.E+tk.S+tk.W)

        self.etabsiscracked=tk.StringVar(value='cracked')
        etabs_wallcracked=tk.Radiobutton(gfetabswall,text='Cracked',value='cracked',variable=self.etabsiscracked)
        etabs_wallcracked.grid(row=1,column=1,padx=5,pady=5)
        etabs_walluncracked=tk.Radiobutton(gfetabswall,text='Uncracked',value='uncracked',variable=self.etabsiscracked)
        etabs_walluncracked.grid(row=2,column=1,padx=5,pady=5)

        #add slab mode options
        getabs_slab=Pmw.Group(gfetabs,tag_text='Slabs in',tagindent=2)
        gfetabs_slab=getabs_slab.interior()
        getabs_slab.grid(row=2,column=3,padx=5,pady=5,sticky=tk.N+tk.E+tk.S+tk.W)
        
        self.etabs_slabdim=tk.StringVar(value='2D')
        etabs_slab2d=tk.Radiobutton(gfetabs_slab,text='2D model',value='2D',variable=self.etabs_slabdim)
        etabs_slab2d.grid(row=1,column=1,padx=5,pady=5)
        etabs_slab3d=tk.Radiobutton(gfetabs_slab,text='3D model',value='3D',variable=self.etabs_slabdim)
        etabs_slab3d.grid(row=2,column=1,padx=5,pady=5)
        
        
        #Construct tab SAP2000
        fsap = Frame(n)
        tsap = n.add(fsap,text='SAP2000')
        #n.tab(fsap).focus_set()
        gtsap=Pmw.Group(fsap,tag_text='SAP2000 Import Options')
        gfsap=gtsap.interior() #the frame of the labeled group
        gtsap.grid(row=2,column=1,padx=5,pady=5)

        impsap=tk.Button(fsap,text="Import SAP2000 Model",width=20,relief=tk.RAISED,command=self.Imp_SAP)
        impsap.grid(row=3,column=1,padx=5,pady=5,sticky=tk.N+tk.E+tk.S+tk.W)

        sap_selfwtmdflbl=tk.Label(gfsap,text='Dead Self Weight Multiplier')
        sap_selfwtmdflbl.grid(row=1,column=1,padx=5,pady=5)
        self.sapselfwtmdf=tk.StringVar(value='0')
        sap_seltwtmdfcmbox=ttk.Combobox(gfsap,width=5,state='readonly',textvariable=self.sapselfwtmdf,values=['0','1'])
        sap_seltwtmdfcmbox.grid(row=1,column=2,padx=5,pady=5)
        #seltwtmdfcmbox.grid_location(x=50,y=50)

        sap_elemdlbl = tk.Label(gfsap,text='Elements Section Property Modifiers')
        sap_elemdlbl.grid(row=2,column=1,padx=5,pady=5)
        self.sapelemd = tk.StringVar(value='All set to 1')
        sap_elemdcmbox = ttk.Combobox(gfsap,state='readonly',textvariable=self.sapelemd,values=['All set to 1','As Per ACI M318 11','Torsional Modifiers Only','Egyptian Standard'],width=25)
        sap_elemdcmbox.grid(row=2,column=2,padx=5,pady=5)

        #add wall crack mode options
        gsapwall=Pmw.Group(gfsap,tag_text='Wall crack mode',tagindent=2)
        gfsapwall=gsapwall.interior()
        gsapwall.grid(row=1,column=3,padx=5,pady=5,sticky=tk.N+tk.E+tk.S+tk.W)

        self.sapiscracked=tk.StringVar(value='cracked')
        sap_wallcracked = tk.Radiobutton(gfsapwall,text='Cracked',value='cracked',variable=self.sapiscracked)
        sap_wallcracked.grid(row=1,column=1,padx=5,pady=5)
        sap_walluncracked=tk.Radiobutton(gfsapwall,text='Uncracked',value='uncracked',variable=self.sapiscracked)
        sap_walluncracked.grid(row=2,column=1,padx=5,pady=5)

        #add slab mode options
        gsap_slab=Pmw.Group(gfsap,tag_text='Slab in',tagindent=2)
        gfsap_slab=gsap_slab.interior()
        gsap_slab.grid(row=2,column=3,padx=5,pady=5,sticky=tk.N+tk.E+tk.S+tk.W)

        self.sap_slabdim=tk.StringVar(value='2D')
        sap_slab2d=tk.Radiobutton(gfsap_slab,text='2D model',value='2D',variable=self.sap_slabdim)
        sap_slab2d.grid(row=1,column=1,padx=5,pady=5)
        sap_slab3d=tk.Radiobutton(gfsap_slab,text='3D model',value='3D',variable=self.sap_slabdim)
        sap_slab3d.grid(row=2,column=1,padx=5,pady=5)
        
        ##__##
        
        #make a menubar with "File" cascade
        menubar = tk.Menu(self.master)
        self.master.config(menu=menubar)
        
        filemenu = tk.Menu(menubar)
        filemenu.add_command(label="Load AutoCAD File",command=self.Load_acadfile)
        filemenu.add_command(label="Exit Cleanly",command=self.on_exit)
        menubar.add_cascade(label="File",menu=filemenu)

        aboutmenu = tk.Menu(menubar)
        aboutmenu.add_command(label="About",command=self.about)
        menubar.add_cascade(label="About",menu=aboutmenu)

    #The method that updates the layers' list
    def updLayers(self):
        if self.acadoc == None:
            self.acadetabscolcmbox['values'] = ['None','0']
        else:
            i = self.acadoc.Layers.Count #get number of layers
            vals = ['None']
            for j in range(0,i):
                vals.append(self.acadoc.Layers.Item(j).Name)
        try:
            self.acadetabscolcmbox['values'] = vals
        except:
            self.acadetabscolcmbox['values'] = ['None','0']
        
    #The method that loads AutoCAD file
    def Load_acadfile(self):
        #get the file's Model Space, & close the previously opened one
        self.oldfilepath = self.filepath.get()
        if self.oldfilepath != "":
            #print "Old Path: ", self.oldfilepath #debug line
            self.acadoc.Close()
            self.acadoc = None
            self.modelspace = None
            
        self.filepath.set(getDwgPath())
        a = self.filepath.get()
        if a:
            self.acadoc=self.acad.Documents.Open(a)
            type(self.acadoc)
            try:
                self.modelspace = self.acadoc.ModelSpace
                showinfo(title=progname,message="File Loaded Successfully")
            except(OSError, COMError):
                showerror(title="Internal Issue",message="Sorry for the accidental issue!\nPlease try to load the file again")
    #The method that imports to ETABS
    def Imp_Etabs(self):
        if self.modelspace != None:
            EtabsImport(self.acadoc,"ETABS",self.swmetabs.get(),self.elemdetabs.get(),self.etabsiscracked.get(),self.etabs_slabdim.get(),self.acadetabscollyr.get())
        else:
            showerror(title=progname,message="AutoCAD drawing isn't loaded yet")

    #The method that imports to SAP2000
    def Imp_SAP(self):
        if self.modelspace != None:
            EtabsImport(self.acadoc,"SAP2000",self.sapselfwtmdf.get(),self.sapelemd.get(),self.sapiscracked.get(),self.sap_slabdim.get(),'')
        else:
            showerror(title=progname,message="AutoCAD drawing isn't loaded yet!",icon=ERROR)
    
    #make the parent window appear on the screen's center
    def centerwin(self):
        w = 800
        h = 400
        sw = self.master.winfo_screenwidth()
        sh = self.master.winfo_screenheight()
        x = (sw - w)/2
        y = (sh - h)/2
        self.master.geometry('%dx%d+%d+%d' % (w,h,x,y))

    def about(self):
        mes = "Utility: CAD2ETABSnSAP\nUtility's Version: 1.0.0\n\nSupported ETABS Version: 16.1.0\nSupported SAP2000 Version: 18.0.1\n\nDeveloper: Eng.Serag Hassouna\nDeveloper's email: serag.hassouna@gmail.com"
        showinfo(title="About",message=mes,icon=INFO)

    def on_exit(self):
        if self.acad:
            self.modelspace = None
            try:
                self.acadoc.Close()
                self.acad.Quit()
            except:
                try:
                    self.acad.Quit()
                except:
                    pass
        try:    
            self.xlwb.Close(SaveChanges=0)
        except:
            pass
        self.xl.Application.Quit()
        #del self.xl
        self.master.destroy()

#Get the Path of .dwg file
def getDwgPath():
    opts = {}
    opts['filetypes'] = [('DWG Files','.dwg'),('DXF Files','.dxf'),('All Files','.*')]
    return askopenfilename(**opts)

#The function that imports from ETABS
def EtabsImport(doc,program,swm,modtypes,wallcrk,slabmode,colyr='None'):
    #Get Etabs or SAP2000 instance, assign it at EtabsObj variable
    try:
        if program == "ETABS":
            EtabsObj = GetActiveObject("CSI.ETABS.API.ETABSObject")
        elif program == "SAP2000":
            EtabsObj = GetActiveObject("CSI.SAP2000.API.SAPObject")
    except(OSError, COMError):
        if program == "ETABS":
            showerror(title=progname,message="ETABS is not running\nPlease open ETABS and try again",icon=ERROR)
        elif program == "SAP2000":
            showerror(title=progname,message="SAP2000 is not running\nPlease open ETABS and try again",icon=ERROR)
        return
    #get model's instance, and initilaize the model
    myModel = EtabsObj.SapModel
    myUnit = 6 #kN_m_C
    myModel.InitializeNewModel(myUnit)
    ret = myModel.File.NewBlank()

    #Define Concrete Materials
    MAT_CONC = 2 #concrete enumeration
    matprop = myModel.PropMaterial

    matdict = doc.Dictionaries.Item("ConcMaterial")
    '''
    Typical values of the material's XRecord, respectively
    Val[0]=label, Val[1]=Fc, Val[2]=E, Val[3]=StrainAtFc, Val[4]=UltimateStrain
    Val[5]=PoisonRatio, Val[6]=ThermalCoeff, Val[7]=UnitWeight
    '''
    i = matdict.Count
    docname = doc.FullName
    for j in range(0,i):
        matXRecord = matdict.Item(j)
        mathandle = matXRecord.ObjectID
        dxfgrcd, val = XRecord_return_1(docname,mathandle,8) #size = 8
        
        ret = matprop.SetMaterial(str(val[0]),MAT_CONC)
        if program == "ETABS":
            ret = matprop.SetOConcrete_1(str(val[0]),val[1],False,0,2,4,val[3],val[4],-0.1)
        elif program == "SAP2000":
            ret = matprop.SetOConcrete_1(str(val[0]),val[1],False,0,2,2,val[3],val[4],-0.1)
        ret = matprop.SetWeightAndMass(str(val[0]),1,val[7])
        ret = matprop.SetMPIsotropic(str(val[0]),val[2],val[5],val[6])
        mes = "Go and see " + str(val[0]) + " material definition"
        showinfo(title="User Debug Work",message=mes,icon=INFO)

    #Define Load Patterns
    DEAD = 1
    LIVE = 3
    OTHER = 8

    if program == "SAP2000":
        ret = myModel.LoadPatterns.ADD("LIVE",LIVE) #SAP2000 doesn't create "LIVE" by default

    loadpatdict = doc.Dictionaries.Item("LoadPatterns")
    icount = loadpatdict.Count
    for i in range(0,icount):
        loadpatXRecord = loadpatdict.Item(i)
        loadpathandle = loadpatXRecord.ObjectID
        dxfgrcd, val = XRecord_return_1(docname,loadpathandle,2)

        #Set the Dead self weight multiplier, and add the rest of the load patterns
        if str(val[0]) == "Dead":
            if program == "ETABS":
                ret = myModel.LoadPatterns.SetSelfWTMultiplier("Dead",float(swm))
            elif program == "SAP2000":
                ret = myModel.LoadPatterns.SetSelfWTMultiplier("DEAD",float(swm))
        elif str(val[0]) == "Live":
            continue
        else:
            if str(val[1]) == "Dead":
                ret = myModel.LoadPatterns.Add(str(val[0]),DEAD)
            elif str(val[1]) == "Live":
                ret = myModel.LoadPatterns.Add(str(val[0]),LIVE)
            elif str(val[1]) == "Other":
                ret = myModel.LoadPatterns.Add(str(val[0]),OTHER)

    #Get modifiers of section properties for frames and slabs
    if modtypes == 'All set to 1':
        beammod = [1,1,1,1,1,1,1,1]
        colmod = [1,1,1,1,1,1,1,1]
        slabmod = [1,1,1,1,1,1,1,1,1,1]
        wallmod = [1,1,1,1,1,1,1,1,1,1]
    elif modtypes == 'As Per ACI M318 11':
        beammod = [1,1,1,0.01,0.35,0.35,1,1]
        colmod = [1,1,1,0.1,0.7,0.7,1,1]
        if slabmode == '2D':
            slabmod = [1,1,1,1,1,1,1,1,1,1]
        elif slabmode == '3D':
            slabmod = [1,1,1,0.25,0.25,1,1,1,1,1]
        if wallcrk == 'cracked':
            wallmod = [1,1,1,0.35,0.35,1,1,1,1,1]
        elif wallcrk == 'uncracked':
            wallmod = [1,1,1,0.7,0.7,1,1,1,1,1]
    elif modtypes == 'Torsional Modifiers Only':
        beammod = [1,1,1,0.01,1,1,1,1]
        colmod = [1,1,1,0.1,1,1,1,1]
        slabmod = [1,1,1,1,1,1,1,1,1,1]
        wallmod = [1,1,1,1,1,1,1,1,1,1]
    elif modtypes == 'Egyptian Standard':
        beammod = [1,1,1,0.01,1,1,1,1]
        colmod = [1,1,1,1,1,1,1,1]
        if slabmode == '2D':
            slabmod = [1,1,1,1,1,1,1,1,1,1]
        elif slabmode == '3D':
            slabmod = [1,1,1,1,0.2,0.2,1,1,1,1]
        if wallcrk == 'cracked':
            wallmod = [1,1,1,0.35,0.35,1,1,1,1,1]
        elif wallcrk == 'uncracked':
            wallmod = [1,1,1,0.7,0.7,1,1,1,1,1]

    #Get frame section properties
    frsecdict = doc.Dictionaries.Item("FrSecProp")
    icount = frsecdict.Count

    '''
    Typical Values
    val[0]=label, val[1]=section shape, val[2]=material, val[3]=section type, val[4]=depth or diameter
    val[5]=breadth or radius, val[6]=unit weight
    '''
    for i in range(0,icount):
        frsecXRecord = frsecdict.Item(i)
        frsecHandle = frsecXRecord.ObjectID
        dxfgrcd, val = XRecord_return_1(docname,frsecHandle,7)
        propfr = myModel.PropFrame

        if str(val[1]) == "Rec":
            ret = propfr.SetRectangle(str(val[0]),str(val[2]),val[4],val[5])
            if str(val[3]) == "Beam":
                ret = propfr.SetRebarBeam(str(val[0]),"A615Gr60","A615Gr60",0.06,0.06,0,0,0,0)
                ret = propfr.SetModifiers(str(val[0]),beammod)
            elif str(val[3]) == "Column":
                ret = propfr.SetRebarColumn(str(val[0]),"A615Gr60","A615Gr60",1,1,0.04,0,3,5,"#20","#10",0.015,0,0,False)
                ret = propfr.SetModifiers(str(val[0]),colmod)
        if str(val[1]) == "Circular":
            ret = propfr.SetCircle(str(val[0]),str(val[2]),str(val[4]))
            if str(val[3]) == "Beam":
                ret = propfr.SetRebarBeam(str(val[0]),"A615Gr60","A615Gr60",0.06,0.06,0,0,0,0)
                ret = propfr.SetModifiers(str(val[0]),beammod)
            elif str(val[3]) == "Column":
                ret = propfr.SetRebarColumn(str(val[0]),"A615Gr60","A615Gr60",1,1,0.04,0,3,5,"#20","#10",0.015,0,0,False)
                ret = propfr.SetModifiers(str(val[0]),colmod)

    #Get slab section properties
    propslab = myModel.PropArea
    slabsecpropdict = doc.Dictionaries.Item("SlabSecProp")
    icount = slabsecpropdict.Count

    SLAB = 0
    DROP = 1
    SHELLTHIN = 1
    SHELLTHICK = 2
    MEMBRANE_ETABS = 3
    MEMBRANE_SAP = 5

    '''
    Typical values:-
    val[0]=label, val[1]=material, val[2]=Etabs thickness, val[3]=SAP2000 thickness, val[4]=unit weight
    '''
    for i in range(0,icount):
        slabsecXRecord = slabsecpropdict.Item(i)
        slabsecHandle = slabsecXRecord.ObjectID
        dxfgrcd, val = XRecord_return_1(docname,slabsecHandle,5)

        if modtypes == 'Egyptian Standard' and program == "SAP2000" and swm == '0':
            thk = val[3]
        else:
            thk = val[2]

        if program == "ETABS":
            ret = propslab.SetSlab(str(val[0]),SLAB,SHELLTHIN,str(val[1]),thk)
        elif program == "SAP2000":
            ret = propslab.SetShell_1(str(val[0]),SHELLTHIN,True,str(val[1]),0,thk,thk)
        ret = propslab.SetModifiers(str(val[0]),slabmod)

    #Get wall section properties [at SAP2000 they will be defined as slab sections]
    wallsecpropdict = doc.Dictionaries.Item("WallSecProps")
    icount = wallsecpropdict.Count

    '''
    Typical values:-
    val[0]=label, val[1]=thickness, val[2]=material
    '''
    for i in range(0,icount):
        wallsecXRecord = wallsecpropdict.Item(i)
        wallsecHandle = wallsecXRecord.ObjectID
        dxfgrcd, val = XRecord_return_1(docname,wallsecHandle,3)

        if program == "ETABS":
            ret = propslab.SetWall(str(val[0]),1,SHELLTHIN,str(val[2]),val[1])
        elif program == "SAP2000":
            ret = propslab.SetShell_1(str(val[0]),MEMBRANE_SAP,True,str(val[2]),0,val[1],val[1])
        ret = propslab.SetModifiers(str(val[0]),wallmod)

    #Get Pier IDs
    if program == "ETABS":
        pieriddict = doc.Dictionaries.Item("PierIDs")
        icount = pieriddict.Count

        for i in range(0,icount):
            pieridXRecord = pieriddict.Item(i)
            pieridHandle = pieridXRecord.ObjectID
            dxfgrcd, val = XRecord_return_1(docname,pieridHandle,1)

            ret = myModel.PierLabel.SetPier(str(val[0]))

    #Get Spandrel IDs
        spandiddict = doc.Dictionaries.Item("SpandralIDs")
        icount = spandiddict.Count

        for i in range(0,icount):
            spandidXRecord = spandiddict.Item(i)
            spandidHandle = spandidXRecord.ObjectID
            dxfgrcd, val = XRecord_return_1(docname,spandidHandle,1)

            ret = myModel.SpandrelLabel.SetSpandrel(str(val[0]),False)

    #From the columns' layer, get base level, then draw the columns (frames and walls)
    if colyr != 'None' and program == "ETABS":
        collayer = doc.Layers.Item(colyr)
        try:
            sscolyr = doc.SelectionSets.Add("ColumnsLayer")
            sshlyr = doc.SelectionSets.Add("ShWallLayer")
        except:
            sscolyr = doc.SelectionSets.Item("ColumnsLayer")
            sshlyr = doc.SelectionSets.Item("ShWallLayer")
        SELECT_ALL = 5
        ftype = array.array('h',[0,8]) #DXF of layers
        fdata = ['LINE',colyr] #columns' layer

        sscolyr.Select(SELECT_ALL,(0,0,0),(0,0,0),ftype,fdata)
        icount = sscolyr.Count
        for i in range(0,icount):
            elem = sscolyr.Item(i)
            
            sp = elem.StartPoint
            ep = elem.EndPoint
            if i == 0:
                baselevel = sp[2] #initialize baselevel from a valid (not hypothetical) value
            baselevel = min(sp[2],ep[2],baselevel)

        try:
            if baselevel == None:
                pass
        except:
            baselevel = None
            
        #get baselevel from Faces (if exist), it's user's responsibility to choose a layer of walls
        #sscolyr.Clear()
        fdata = ['3DFACE',colyr]
        sshlyr.Select(SELECT_ALL,(0,0,0),(0,0,0),ftype,fdata)
        icount = sshlyr.Count
        for i in range(0,icount):
            elem = sshlyr.Item(i)

            V1 = elem.Coordinate(0)
            V2 = elem.Coordinate(1)
            V3 = elem.Coordinate(2)
            V4 = elem.Coordinate(3)

            if i == 0 and baselevel == None:
                baselevel = V1[2]
            baselevel = min(V1[2],V2[2],V3[2],V4[2],baselevel)
        try:
            myModel.Story.SetElevation("Base",baselevel)
        except:
            showerror(title=progname,message="Columns Layer has no column!\nClose the generated model and try again")
            return
        
        retprog = drawlines(docname,sscolyr,colyr,myModel,program,swm,False) #draw the currently selected lines
        if retprog == 0:
            return
        retprog = drawFaces(doc,docname,sshlyr,colyr,myModel,program,swm,False) #draw the currently selected Faces
        if retprog == 0:
            return
        sscolyr.Delete()
        sshlyr.Delete()

    #Draw the rest of lines and 3DFaces
    try:
        lines = doc.SelectionSets.Add("Frames")
        Faces = doc.SelectionSets.Add("Shells")
        Points = doc.SelectionSets.Add("Points")
    except:
        #the "try .. except" is to insure using the same selection set among multiple sessions without error
        lines = doc.SelectionSets.Item("Frames")
        Faces = doc.SelectionSets.Item("Shells")
        Points = doc.SelectionSets.Item("Points")
    icount = doc.Layers.Count
    for i in range(0,icount):
        layer = doc.Layers.Item(i)
        layername = layer.Name

        if layername != colyr:
            retprog = drawlines(docname,lines,layername,myModel,program,swm,True)
            if retprog == 0:
                return
            retprog = drawFaces(doc,docname,Faces,layername,myModel,program,swm,True)
            if retprog == 0:
                return
            drawPoints(docname,Points,layername,myModel,True)

    ret = myModel.View.RefreshView(0,True)
    showinfo(title=progname,message="Work is Done!") #importing is successful

#A utility function to draw lines from a given layer and a selection set to be used
#Specify if the selection set must be cleared or not
def drawlines(docname,sslines,layer,sapmodel,program,swm,clear=True):
    grname = layer + '_' + 'LINES'

    ret = sapmodel.GroupDef.SetGroup(grname,-1,True,True,True,True,True,True,True,True,False,False,True) #create a group for this layer
    ftype = array.array('h',[0,8])
    fdata = ['LINE',layer]
    SELECT_ALL = 5
    if clear:
        sslines.Clear()
        sslines.Select(SELECT_ALL,(0,0,0),(0,0,0),ftype,fdata)
    icount = sslines.Count
    for i in range(0,icount):
        elem = sslines.Item(i)
        elemname = layer + '_Fr ' + str(i)
        sp = elem.StartPoint
        ep = elem.EndPoint

        elemxdict = elem.GetExtensionDictionary()
        try:
            elemdistloads = elemxdict.Item("DistLoads") #this is a dictionary of all assigned loads
            jcount = elemdistloads.Count
        except:
            pass #assignment of distributed loads is not mandatory any more 
        try:
            elemsecprop = elemxdict.Item("SecProp") #this is an XRecord of the section property
        except:
            showerror(title=progname,message="There exist line(s) with no assigned section property\nCheck your AutoCAD drawing,\nClose the generated model and try again")
            return 0 #failed
        secpropHandle = elemsecprop.ObjectID
        dxfgrcd,valprop = XRecord_return_1(docname,secpropHandle,1)
        '''
        Typical values:-
        valprop[0]=section property name
        '''
        ret = sapmodel.FrameObj.AddByCoord(sp[0],sp[1],sp[2],ep[0],ep[1],ep[2],elemname,valprop[0],elemname,"Global") #add by coordinate
        ret = sapmodel.FrameObj.SetGroupAssign(elemname,grname,False,0) #assign to its special group
        
        #assign distributed load, but consider that it's not mandatory to be assigned at the drawing
        try:
            for j in range(0,jcount):
                elemload = elemdistloads.Item(j)
                elemloadHandle = elemload.ObjectID
                dxfgrcd,vaload = XRecord_return_1(docname,elemloadHandle,4)
                if int(vaload[2]) in [1,2,3]:
                    cs = "Local"
                else:
                    cs = "Global"
                '''
                Typical values:-
                vaload[0]=start value, vaload[1]=end value, vaload[2]=direction, vaload[3]=load pattern
                '''
                if str(vaload[3]) == "Dead":
                    if swm == '0':
                        if program == "ETABS":
                            ret = sapmodel.FrameObj.SetLoadDistributed(elemname,"Dead",1,int(vaload[2]),0,1,vaload[0],vaload[1],cs,True,True,0)
                        elif program == "SAP2000":
                            if vaload[2] == 6:
                                ret = sapmodel.FrameObj.SetLoadDistributed(elemname,"DEAD",1,10,0,1,vaload[0],vaload[1],cs,True,True,0)
                            elif vaload[2] == 9:
                                ret = sapmodel.FrameObj.SetLoadDistributed(elemname,"DEAD",1,11,0,1,vaload[0],vaload[1],cs,True,True,0)
                            else:
                                ret = sapmodel.FrameObj.SetLoadDistributed(elemname,"DEAD",1,int(vaload[2]),0,1,vaload[0],vaload[1],cs,True,True,0)
                    elif swm == '1':
                        continue
                elif str(vaload[3]) == "Live":
                    if program == "ETABS":
                        ret = sapmodel.SetLoadDistributed(elemname,"Live",int(vaload[2]),0,1,vaload[0],vaload[1],cs,True,True,0)
                    elif program == "SAP2000":
                        if vaload[2] == 6:
                            ret = sapmodel.FrameObj.SetLoadDistributed(elemname,"LIVE",1,10,0,1,vaload[0],vaload[1],cs,True,True,0)
                        elif vaload[2] == 9:
                            ret = sapmodel.FrameObj.SetLoadDistributed(elemname,"LIVE",1,11,0,1,vaload[0],vaload[1],cs,True,True,0)
                        else:
                            ret = sapmodel.FrameObj.SetLoadDistributed(elemname,"LIVE",1,int(vaload[2]),0,1,vaload[0],vaload[1],cs,True,True,0)
                else:
                    if program == "ETABS":
                        ret = sapmodel.FrameObj.SetLoadDistributed(elemname,str(vaload[3]),1,int(vaload[2]),0,1,vaload[0],vaload[1],cs,True,True,0)
                    elif program == "SAP2000" and str(vaload[3]) != "DEAD":
                        if vaload[2] == 6:
                            ret = sapmodel.FrameObj.SetLoadDistributed(elemname,str(vaload[3]),1,10,0,1,vaload[0],vaload[1],cs,True,True,0)
                        elif vaload[2] == 9:
                            ret = sapmodel.FrameObj.SetLoadDistributed(elemname,str(vaload[3]),1,11,0,1,vaload[0],vaload[1],cs,True,True,0)
                        else:
                            ret = sapmodel.FrameObj.SetLoadDistributed(elemname,str(vaload[3]),1,int(vaload[2]),0,1,vaload[0],vaload[1],cs,True,True,0)
                
        except:
            pass
    return 1 #passed

#A utility function to draw 3Faces from a given layer and a selection set to be used
def drawFaces(doc,docname,sslines,layer,sapmodel,program,swm,clear=True):
    grname = layer + '_' + 'Shells'

    ret = sapmodel.GroupDef.SetGroup(grname,-1,True,True,True,True,True,True,True,True,False,False,True) #create a group for this layer
    ftype = array.array('h',[0,8])
    fdata = ['3DFACE',layer]
    SELECT_ALL = 5
    if clear:
        sslines.Clear()
        sslines.Select(SELECT_ALL,(0,0,0),(0,0,0),ftype,fdata)
    icount = sslines.Count
    for i in range(0,icount):
        elem = sslines.Item(i)
        elemname = layer + '_Sh ' + str(i)
        V1 = elem.Coordinate(0)
        V2 = elem.Coordinate(1)
        V3 = elem.Coordinate(2)
        V4 = elem.Coordinate(3)

        elemX = [V1[0],V2[0],V3[0],V4[0]]
        elemY = [V1[1],V2[1],V3[1],V4[1]]
        elemZ = [V1[2],V2[2],V3[2],V4[2]]

        elemxdict = elem.GetExtensionDictionary()
        try:
            elemdistload = elemxdict.Item("DistLoads") #this is a dictionary of distributed loads
        except:
            try:
                elemdistload = elemxdict.Item("WallDistLoads")
            except:
                pass #assignment of distributed load is not mandatory any more
        
        try:
            elemsecprop = elemxdict.GetObject("SecProp") #this is an XRecord of slab's section property [make AutoCAD assigns them to walls of in-draw-shw then return to here]
            elemtype = "slab"
        except:
            try:
                elemsecprop = elemxdict.GetObject("WallProp")
                elemtype = "wall"
            except:
                showerror(title=progname,message="There exist shell(s) with no assigned section property\nCheck your AutoCAD drawing,\nClose the generated model and try again")
                return 0 #failed
        
        #secpropHandle = elemsecprop.Handle
        secpropobjid = elemsecprop.ObjectID #debugging trial
        
        #dxfgrcd, valprop = XRecord_return(docname,secpropHandle,1) #produces error unknown handle, it seems that HandleToObject AutoCAD's method contains a bug
        dxfgrcd, valprop = XRecord_return_1(docname,secpropobjid,1) #debugging trial
        
        '''
        Typical values:-
        val[0] = section property
        '''
        ret = sapmodel.AreaObj.AddByCoord(4,elemX,elemY,elemZ,elemname,str(valprop[0]),elemname) #add by coordinates
        ret = sapmodel.AreaObj.SetGroupAssign(elemname,grname,False,0)
        #Assign distributed loads
        #elemtype = slabORwall(str(valprop[0]),doc)
        try:
            jcount = elemdistload.Count
            for j in range(0,jcount):
                distload = elemdistload.Item(j)
                #distloadHandle = distload.Handle
                distloadobjid = distload.ObjectID
                #dxfgrcd, vaload = XRecord_return(docname,str(distloadHandle),3)
                dxfgrcd, vaload = XRecord_return_1(docname,distloadobjid,3) #debugging trial
                if int(vaload[1]) in [1,2,3]:
                    cs = "Local"
                else:
                    cs = "Global"
                '''
                Typical values:-
                vaload[0]=load value, vaload[1]=direction, vaload[2]=load pattern
                '''
                if str(vaload[2]) == "Dead":
                    if swm == '0':
                        if program == "ETABS" and elemtype == "slab":
                            ret = sapmodel.AreaObj.SetLoadUniform(elemname,"Dead",vaload[0],int(vaload[1]),True,cs,0)
                        elif program == "SAP2000" and elemtype == "slab":
                            if vaload[1] == 6:
                                ret = sapmodel.AreaObj.SetLoadUniform(elemname,"DEAD",vaload[0],10,True,cs,0)
                            elif vaload[1] == 9:
                                ret = sapmodel.AreaObj.SetLoadUniform(elemname,"DEAD",vaload[0],11,True,cs,0)
                            else:
                                ret = sapmodel.AreaObj.SetLoadUniform(elemname,"DEAD",vaload[0],int(vaload[1]),True,cs,0)
                        elif elemtype == "wall":
                            continue
                    elif swm == '1':
                        continue
                if str(vaload[2]) == "Live":
                    if program == "ETABS":
                        ret = sapmodel.AreaObj.SetLoadUniform(elemname,"Live",vaload[0],int(vaload[1]),True,cs,0)
                    elif program == "SAP2000":
                        if vaload[1] == 6:
                            ret = sapmodel.AreaObj.SetLoadUniform(elemname,"LIVE",vaload[0],10,True,cs,0)
                        elif vaload[1] == 9:
                            ret = sapmodel.AreaObj.SetLoadUniform(elemname,"LIVE",vaload[0],11,True,cs,0)
                        else:
                            ret = sapmodel.AreaObj.SetLoadUniform(elemname,"LIVE",vaload[0],int(vaload[1]),True,cs,0)
                else:
                    if program == "ETABS":
                        ret = sapmodel.AreaObj.SetLoadUniform(elemname,str(vaload[2]),vaload[0],int(vaload[1]),True,cs,0)
                    elif program == "SAP2000":
                        if vaload[1] == 6:
                            ret = sapmodel.AreaObj.SetLoadUniform(elemname,str(vaload[2]),vaload[0],10,True,cs,0)
                        elif vaload[1] == 9:
                            ret = sapmodel.AreaObj.SetLoadUniform(elemname,str(vaload[2]),vaload[0],11,True,cs,0)
                        else:
                            ret = sapmodel.AreaObj.SetLoadUniform(elemname,str(vaload[2]),vaload[0],int(vaload[1]),True,cs,0)
        except:
            pass #assignment of distributed load is not mandatory any more
        #Assign Pier ID
        try:
            elempierid = elemxdict.Item("PierID")
            #pieridHandle = elempierid.Handle
            pieridobjid = elempierid.ObjectID
            #dxfgrcd, val = XRecord_return(docname,pieridHandle,1)
            dxfgrcd, val = XRecord_return_1(docname,pieridobjid,1) #debugging trial
            if str(val[0]) != "None":
                ret = sapmodel.AreaObj.SetPier(elemname,str(val[0]),0)
        except:
            pass

        #Assign Spandrel ID
        try:
            elemspandid = elemxdict.Item("SpandralID")
            #spandidHandle = elemspandid.Handle
            spandidobjid = elemspandid.ObjectID
            #dxfgrcd, val = XRecord_return(docname,spandidHandle,1)
            dxfgrcd, val = XRecord_return_1(docname,spandidobjid,1) #debugging trial
            if str(val[0]) != "None":
                ret = sapmodel.AreaObj.SetSpandrel(elemname,str(val[0]),0)
        except:
            pass

    return 1 #passed     

#A utility function that determines if the shell element belongs to a slab or a wall
def slabORwall(secprop,doc):
    try:
        doc.Dictionary.Item("SlabSecProp").Item(secprop)
        return "slab"
    except:
        try:
            doc.Dictionaries.Item("WallSecProps").Item(secprop)
            return "wall"
        except:
            return "slab"

#A utility function to draw points and assign their restrains
def drawPoints(docname,sslines,layer,sapmodel,clear=True):
    grname = layer + '_' + 'POINTS'

    ret = sapmodel.GroupDef.SetGroup(grname,-1,True,True,True,True,True,True,True,True,False,False,True) #create a group for this layer
    ftype = array.array('h',[0,8])
    fdata = ['POINT',layer]
    SELECT_ALL = 5
    if clear:
        sslines.Clear()
        sslines.Select(SELECT_ALL,(0,0,0),(0,0,0),ftype,fdata)
    icount = sslines.Count
    for i in range(0,icount):
        elem = sslines.Item(i)
        elemname = layer + '_Po ' + str(i)
        coord = elem.Coordinates

        try:
            elemxdict = elem.GetExtensionDictionary()
            xrestrain = elemxdict.Item("Restrain")
            ret = sapmodel.PointObj.AddCartesian(coord[0],coord[1],coord[2],elemname,elemname,"Global",False,0)
            ret = sapmodel.PointObj.SetGroupAssign(elemname,grname,False,0)
            xrestrainHandle = xrestrain.ObjectID
            dxfgrcd, val = XRecord_return_1(docname,xrestrainHandle,1)
            if val[0] == 'Hinged':
                restrain = [True,True,True,False,False,False]
            elif val[0] == 'Fixed':
                restrain = [True,True,True,True,True,True]
            ret = sapmodel.PointObj.SetRestraint(elemname,restrain,0)
        except:
            pass #assignment of restrain type is not mandatory anymore

#The function that imports from SAP2000

#def SAPImport(doc):
#    
#    name = doc.FullName
#    matdict = doc.Dictionaries.Item("ConcMaterial")
#    handletest = matdict.Item(0).Handle
#    dxfgrcd,vals = XRecord_return(name,handletest,8)
#    print "Group Codes: ", dxfgrcd
#    print "Values: ", vals
#    print "String Type: ", type(vals[0])
#    print "Numbers Type: ", type(vals[1])
#    print "Vals[1]= ", str(vals[0])
#    print "__________"
    
#A utility function that takes the handle of an XRecord and its data size, and return its data and dxf group codes
def XRecord_return(namefile,handle,size):
    xl.Range["A1"].Value[xlRangeValueDefault] = namefile
    xl.Range["A2"].Value[xlRangeValueDefault] = handle
    xl.Application.Run("facilitate.xlsm!import_sap_etabs.text")

    dxfgrcd = []
    vals = []
    for i in range(0,size):
        CellB = 'B' + str(i+1)
        CellC = 'C' + str(i+1)
        dxfgrcd.append(xl.Range[CellB].Value[xlRangeValueDefault])
        vals.append(xl.Range[CellC].Value[xlRangeValueDefault])

    return dxfgrcd,vals

#A utility function that takes the Object ID of an XRecord and its data size, and return its data and dxf group codes
def XRecord_return_1(namefile,objid,size):
    xl.Range["A1"].Value[xlRangeValueDefault] = namefile
    xl.Range["A2"].Value[xlRangeValueDefault] = objid
    xl.Application.Run("facilitate.xlsm!import_sap_etabs.getfromid")

    dxfgrcd = []
    vals = []
    for i in range(0,size):
        CellB = 'B' + str(i+1)
        CellC = 'C' + str(i+1)
        dxfgrcd.append(xl.Range[CellB].Value[xlRangeValueDefault])
        vals.append(xl.Range[CellC].Value[xlRangeValueDefault])

    return dxfgrcd,vals

#Construct the program itself
def main():
    
    #Run the GUI
    root = tk.Tk()
    Pmw.initialise(root)
    #supply all buttons' functions to MYWINDOW
    app = MYWINDOW()
    root.mainloop()

#Execute the program
if __name__ == '__main__':
    main()
