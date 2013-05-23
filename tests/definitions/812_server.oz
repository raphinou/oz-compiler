% Server with port object
local
   Xform
   XformPipeMaker
   NewPortObject2
   ServerProc
   ClientProc
   Server
   Client
   Res
in




   proc {Xform As Bs Cs Ds}
      proc {XformLoop As Bs Cs Ds}
        case As#Bs of (A|Ar)#(B|Br) then
          C|Cr=Cs
          D|Dr=Ds
        in
          C=B
          D=B-A
          {XformLoop Ar Br Cr Dr}
        end
      end
   in
      thread {XformLoop As Bs Cs Ds} end
   end

   fun {XformPipeMaker N}
      proc {XformPMLoop N As Bs Cs Ds}
        if N==0 then
          As=Cs Bs=Ds
        else A1s B1s in
          {Xform As Bs A1s B1s}
          {XformPMLoop N-1 A1s B1s Cs Ds}
        end
      end
   in
      proc {$ As Bs Cs Ds}
        {XformPMLoop N As Bs Cs Ds}
      end
   end

   local
   As Bs Cs Ds in
   {{XformPipeMaker 5} As Bs Cs Ds}

   As=1|2|3|4|_
   Bs=5|6|7|8|_



   fun {NewPortObject2 Proc}
      Sin in
      thread for M in Sin do {Proc M} end end
      {NewPort Sin}
   end


   proc {ServerProc Msg}
      case Msg
      of calc(X Y Client) then X1 D in
        {Send Client delta(D)}
        thread
          X1=X+D
          Y=X1*X1+2.0*X1+2.0
        end
      end
   end
   Server={NewPortObject2 ServerProc}

   proc {ClientProc Msg}
      case Msg
      of work(Y) then Y1 Y2 in
        {Send Server calc(10.0 Y1 Client)}
        {Send Server calc(20.0 Y2 Client)}
        thread Y=Y1+Y2 end
      [] delta(D) then
        D=1.0
      end
   end
   Client={NewPortObject2 ClientProc}

   Res={Send Client work($)}
   {Wait Res}
   {Show Res}
   end
end

