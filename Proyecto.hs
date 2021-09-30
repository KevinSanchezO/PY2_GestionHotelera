import System.IO
import Control.DeepSeq
import Control.Exception
import Data.Time.Clock 
import Data.Time.Calendar 
import Data.Time.LocalTime



{-----Menu de inicio----
Menu el cual se mostrara primero al iniciar el programa-}
menuPrincipal::IO()
menuPrincipal=
    do
        putStrLn "\nBienvenido al menu principal\n 1. Opciones Administrativas\n 2. Opciones Generales\n 0.Salir\n Digite una opcion: "
        opcion<-getLine
        case opcion of
            "1" -> menuAdministrativo
            "2"->menuOpcionesGenerales
            "0"->resetearArchivo "validar.txt" "T"
            _ -> errorDigito


{-----Menu de opciones administrativas----
Menu donde se permite acceder a las opciones administrativas del sistema-}
menuAdministrativo :: IO()
menuAdministrativo= 
    do 
        putStrLn "\n\n*****Opciones Administrativas*****\n 1.Informacion de hotel\n 2.Cargar tipo de habitaciones\n 3.Asignar cantidad de habitaciones por tipo\n 4.Carga de tarifas\n 5.Consultar reservacion\n 6.Consulta de facturas\n 7.Estadisticas de ocupacion\n 0.Volver\n Digite una opcion: "
        s<-getLine
        case s of
            "1" ->infoHoteles
            "2"->cargarTipoHabitacion
            "3"->validarUnaVezCantidadTipo
            "4"->cargarTarifas
            "5"->consultarReservaciones
            {-"6"->consultaFacturas
            "7"->estadisticasOcupacion-}
            "0"->menuPrincipal
            _ -> errorDigito


{-----Menu de opciones generales----
Menu donde se permite acceder a las opciones generales del sistema-}
menuOpcionesGenerales:: IO()
menuOpcionesGenerales=
    do
        putStrLn "\n\n******Opciones Generales*****\n 1.Reservacion\n 2.Cancelar Reservacion\n 3.Facturar Reservacion\n 0.Salir\n Digite una opcion: "
        d<-getLine
        case d of
            "1"->reservacion
            "2"->cancelarReservacion
            {-"3"->facturarReservacion-}
            "0"->menuPrincipal
            _ ->errorDigito


{-----errorDigito----
Menu el cual se mostrara primero al iniciar el programa-}
errorDigito::IO()
errorDigito=
    do
        putStr "\ESC[2J"
        putStrLn "\nERROR, Digite una opcion valida\n"
        menuPrincipal

{-------------------------------------------------------INFORMACION DE HOTELES-------------------------------------------------------------------}

{----infoHoteles----
Muestra los datos del hotel administrativo leyendo el archivo que almacena los datos--}
infoHoteles:: IO()
infoHoteles=
    do
        putStrLn "\n_____________________INFORMACION DEL HOTEL_____________________"
        cs <- readFile "infoHoteles.txt"
        let datos = split ',' cs
        mostrarAux datos

{----mostrarAux----
Menu para mostrar de forma tabulada los datos del archivo--}
mostrarAux::[String]->IO()
mostrarAux [] = menuAdministrativo
mostrarAux datosArchivo = 
    do
        let nombre = head datosArchivo
        let mensajeNombre = "Nombre: "++nombre
        let resto1 = tail datosArchivo

        let cedula = head resto1
        let mensajeCedula = "Cedula Juridica: "++cedula 
        let resto2 = tail resto1 

        let sitioWeb = head resto2
        let mensajeSitioWeb = "Sitio Web: "++sitioWeb
        let resto3 = tail resto2

        let telefono = head resto3
        let mensajeTelefono = "Telefono: "++telefono
        let resto4 = tail resto3

        let pais = head resto4
        let mensajePais = "Pais: "++pais
        let resto5 = tail resto4

        let provincia = head resto5
        let mensajeProvincia = "Provincia: "++provincia

        putStrLn mensajeNombre
        putStrLn mensajeCedula
        putStrLn mensajeSitioWeb
        putStrLn mensajeTelefono
        putStrLn mensajePais
        putStrLn mensajeProvincia
        menuAdministrativo

{----split----
Separa en listas un string con un valor determinado tipo char--}
split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
    (ls, "") -> [ls]
    (ls, x:rs) -> ls : split c rs

{-------------------------------------------------------CARGA DE TIPO DE HABITACION-------------------------------------------------------------------}

{----cargarTipoHabitaciones----
solicita al usuario la ruta del archivo con los datos de habitaciones que desea ingresar
y administra el proceso de guardar dichos datos-}
cargarTipoHabitacion::IO()
cargarTipoHabitacion =
    do 
        putStrLn "\nIngrese la ruta del archivo con los tipos de habitaciones: "
        ruta <- getLine
        listaA <- leerArchivo "habitacionesCargadas.txt"

        contenido <- readFile ruta
        let listaB = split ',' contenido
         
        let listaC = listaNoRepetidos listaB listaA 0
        resetearArchivo "habitacionesCargadas.txt" ""
        escribirEnHabitaciones "habitacionesCargadas.txt" listaC

        menuAdministrativo
        
{----leerArchivo----
Permite cargar en una lista los datos de un archivo cuyos datos esten separados por
saltos de linea haciendo cada salto de linea un dato dentro de la lista-}
leerArchivo :: System.IO.FilePath -> IO [String]
leerArchivo (archivo) = 
    do
        contents <- readFile archivo
        evaluate (force contents)
        let lineas = lines contents
        return lineas;

{----listaNoRepetidos----
verifica que en el archivo donde persisten los datos de las habitaciones
no existan nombre de habitaciones repetidos
-}
listaNoRepetidos :: [String] -> [String] -> Int ->[String]
listaNoRepetidos [] res opc = res
listaNoRepetidos list res opc = 
    do
        let listaCortada = tail list
        let listaCortada1 = tail listaCortada
        let listaCortada2 = tail listaCortada1
        let listaCortada3 = tail listaCortada2
        let hd = head list
        let res2 = res ++ [hd]
        let opc1 = opc+1
        let opc2 = opc+3
        (if ((mod opc  3) == 0) && encontrarEnLista hd res then
             listaNoRepetidos listaCortada2 res opc2
        else
            listaNoRepetidos listaCortada res2 opc1)

{----resetearArchivo----
Permite dejar en blanco un archivo de texto para volver a escribir su informacion
modificada-}
resetearArchivo :: System.IO.FilePath -> String -> IO ()
resetearArchivo archivo info = 
    do
        writeFile archivo (info)

{----encontrarEnLista----
verifica si en una lista se encuentra un valor en especifico-}
encontrarEnLista :: String->[String]->Bool
encontrarEnLista nombre [] = False
encontrarEnLista nombre lista = 
    do
        let primerElemento = head lista
        let listaCortada = tail lista
        if primerElemento == nombre then
            True
        else
            encontrarEnLista nombre listaCortada


{----escribirEnHabitaciones----
se encarga de administrar la escritura de los datos de habitaciones sin que
se repita datos de habitaciones en el archivo de la persistencia de datos-}
escribirEnHabitaciones :: System.IO.FilePath->[String]->IO ()
escribirEnHabitaciones archivo [] = menuAdministrativo
escribirEnHabitaciones archivo lista = 
    do
        let dato = head lista
        let resto = tail lista

        let guardarMensaje = dato  ++ "\n"
        appendFile archivo guardarMensaje
        escribirEnHabitaciones archivo resto
       
{--------------------------------------------------------------------------------------------------------------------------}

stringToInt texto = 
    do
        let valor = read texto::Integer
        return valor
        

intToString numero = 
    do
        let valor = show numero
        return valor

{------------------------------------------------CANTIDAD HABITACIONES POR TIPO---------------------------------------------}

validarUnaVezCantidadTipo::IO()
validarUnaVezCantidadTipo = 
    do
        validacion <- leerArchivo "validar.txt"
        let valido = head validacion
        if (valido == "T") then
            asignarCantidadHabitacionesPorTipo
        else
            negacionCargarTipoHabitacion


negacionCargarTipoHabitacion::IO()
negacionCargarTipoHabitacion =
    do
        putStrLn "\nYa fueron generadas las habitaciones, no se puede realizar nuevamente.\n"
        menuAdministrativo


{----asignarCantidadHabitacionesPorTipo----
Administra todos los archivos necesarios para poder hacer esta funcionalidad-}
asignarCantidadHabitacionesPorTipo:: IO()
asignarCantidadHabitacionesPorTipo = 
    do
        listaHabitacion <- leerArchivo"habitacionesCargadas.txt"
        let listaHab = crearListaHabitacionesAux listaHabitacion []
        resetearArchivo "cantidadPorTiposHabitaciones.txt" ""
        resetearArchivo "codigosTiposHabitaciones.txt" ""
        asignarCantidadesDeHabiatciones "cantidadPorTiposHabitaciones.txt" listaHab

{----asignarCantidadesDeHabiatciones----
Le solicita al usuario la cantidad de habitaciones que se crearan por tipo almacenado-}
asignarCantidadesDeHabiatciones:: System.IO.FilePath->[String]->IO()
asignarCantidadesDeHabiatciones archivo [] = mostrarCantidades
asignarCantidadesDeHabiatciones archivo listahab = 
    do
        let nombre = head listahab
        let resto = tail listahab
        let mensaje = "\tDigite cuantas habitaciones tendra el hotel de este tipo ->" ++ nombre ++ " : "
        putStrLn mensaje
        cantidad <- getLine
        appendFile archivo (nombre ++ "\n")
        appendFile archivo (cantidad ++ "\n")
        cantidad2 <- stringToInt(cantidad)
        agregarToHabitaciones archivo nombre cantidad2 0 resto "codigosTiposHabitaciones.txt"

{-----agregarToHabitaciones----
Genera el codigo unico de las habitaciones creadas-}
agregarToHabitaciones:: System.IO.FilePath->String->Integer->Integer->[String]->System.IO.FilePath->IO()
agregarToHabitaciones archivo nombre cantidad contador lista archivoCodigos = 
    do 
        let contador2 = contador + 1
        pContador2 <- intToString contador
        let catidadDefinitiva = nombre++pContador2++"\n"
        if contador < cantidad then
            escribirEnCantidadPorTipo archivo nombre cantidad contador2 lista catidadDefinitiva archivoCodigos
        else
            asignarCantidadesDeHabiatciones archivo lista

{-----escribirEnCantidadPorTipo-----
Agrega al archivo de la persistencia de datos el codigo de una habitacion creada.-}
escribirEnCantidadPorTipo::System.IO.FilePath->String->Integer->Integer->[String]->String->System.IO.FilePath->IO()
escribirEnCantidadPorTipo archivo nombre cant cont2 lista catidadDefinitiva archivo2=
    do 
        appendFile archivo2 catidadDefinitiva 
        agregarToHabitaciones archivo nombre cant cont2 lista archivo2
    
{----mostrarCantidades----
Se encarga de preparar los archivos para mostrar las cantidades de habitaciones creadas-}
mostrarCantidades::IO()
mostrarCantidades = 
    do
        listaCodigosTipoHabitacion <- leerArchivo "codigosTiposHabitaciones.txt"
        listaCantidadTiposHabitacion<- leerArchivo "cantidadPorTiposHabitaciones.txt"
        mostrarCantidadesAux listaCantidadTiposHabitacion listaCodigosTipoHabitacion

{----mostrarCantidadesAux----
Muestra el nombre y el tipo de habitacion para luego mostrar la cantidades de ese tipo de habitacion-}
mostrarCantidadesAux::[String]->[String]->IO()
mostrarCantidadesAux [] [] = negarHabitacionesPorTipo
mostrarCantidadesAux listaCodigosTipoHabitacion listaCantidadTiposHabitacion = 
    do
        let nombre = head listaCodigosTipoHabitacion
        let corte = tail listaCodigosTipoHabitacion 
        let corte2 = tail corte
        let cantidad = read(head corte)::Int
        let mensaje = "El tipo "++nombre++" tiene las siguientes habitaciones e identificadores: \n"
        putStrLn mensaje
        mostrarSig corte2 cantidad 0 listaCantidadTiposHabitacion


{----mostrarSig----
muestra la cantidad de habitaciones por tipo y continua la funcion recursiva 
de mostrar las cantidades de habitaciones por tipo-}
mostrarSig::[String]->Int->Int->[String]->IO()
mostrarSig lista cant cont listaCantidadTiposHabitacion = 
    do
        let hd = head listaCantidadTiposHabitacion
        let corte = tail listaCantidadTiposHabitacion
        let cont2 = cont+1
        putStrLn (hd++"\n")
        if cont == cant-1 then 
            mostrarCantidadesAux lista corte
        else
            mostrarSig lista cant cont2 corte

negarHabitacionesPorTipo::IO()
negarHabitacionesPorTipo = 
    do
        resetearArchivo "validar.txt" "F"
        menuAdministrativo

{-----------------------------------------------------------CARGAR TARIFAS---------------------------------------------------------------}

{----cargarTarifas-----
Se encarga de preparar los archivos necesarios para esta funcionalidad para
realizar la persistencia de datos-}
cargarTarifas::IO()
cargarTarifas = 
    do
        resetearArchivo "Tarifas.txt" ""
        list <- leerArchivo "habitacionesCargadas.txt"
        let listaDeHabitaciones = crearListaHabitacionesAux list []
        cargarTarifasAux listaDeHabitaciones
       
{----cargarTarifasAux----
Le solicita al usario la tarifa o precio de cada tipo de habitacion para
luego guardar cada tarifa en el archivo de persistencia de datos-}
cargarTarifasAux ::[String]->IO()
cargarTarifasAux [] = menuAdministrativo 
cargarTarifasAux lista = 
    do
        let tipo = head lista
        let tlLista = tail lista
        let interaccion = ("\nIngrese un precio para la habitacion de tipo " ++ tipo ++ " : ")
        putStrLn interaccion
        precio <- getLine
        appendFile "Tarifas.txt" (tipo ++ "\n" ++ precio ++ "\n")
        cargarTarifasAux tlLista     

{----crearListaHabitacionesAux----
crea una lista con los nombres de los tipos de habitaciones cargadas-}
crearListaHabitacionesAux:: [String]->[String]->[String]
crearListaHabitacionesAux [] listaDeHabitaciones = listaDeHabitaciones
crearListaHabitacionesAux lista listaDeHabitaciones = 
    do
        let nombre = head lista
        let valor_1 = tail lista
        let valor2 = tail valor_1
        let listaSigHabitacion = tail valor2
        let listaDeHabitaciones2 = listaDeHabitaciones++[nombre] 
        crearListaHabitacionesAux listaSigHabitacion listaDeHabitaciones2


{-----------------------------------------------------------Reservacion-----------------------------------------------------}

{----reservacion----
solicita los inputs del usuario sobre la fecha de entrada y salida, cantidad de adultos, cantidad
de niños y el nombre de la persona que reserva-}
reservacion::IO()
reservacion = 
    do

        putStrLn "Digite la fecha de ingreso"
        fechaI <- getLine

        putStrLn "Digite la fecha de salida"
        fechaS <- getLine

        putStrLn "Digite cantidad de adultos"
        adultos <- getLine

        putStrLn "Digite la cantidad de niños"
        ninos <- getLine

        putStrLn "Digite el nombre de la persona que reserva"
        nombre <- getLine

        let fechaEntrada = split '/' fechaI

        let dia= head fechaEntrada
        let corte= tail fechaEntrada
        let mes= head corte
        let corte2=tail corte
        let anio= head corte2

        let fechaSalida = split '/' fechaS

        let diaS= head fechaSalida
        let corte3= tail fechaSalida
        let mesS= head corte3
        let corte4=tail corte3
        let anioS= head corte4


        let anioIngreso = read anio::Int
        let anioSalida = read anioS::Int
        let mesIngreso = read mes::Int
        let mesSalida = read mesS ::Int
        let diaIngreso = read dia ::Int
        let diaSalida = read diaS ::Int

        listaHabitaciones <- leerArchivo "habitacionesCargadas.txt" 

        if validarFechaDigitos diaIngreso mesIngreso anioIngreso diaSalida mesSalida anioSalida then
            if  validarFecha diaIngreso mesIngreso anioIngreso diaSalida mesSalida anioSalida then 
                reservarHabitacion fechaI fechaS adultos ninos nombre listaHabitaciones 
            else 
                fechaInvalida
        else
            fechaInvalida


{----validarFecha----
Valida que los valores de la fecha de salida y entrada sean validos, de tal forma que
no se pueda tener un fecha de salida menor a la fecha de entrada-}
validarFecha::Int->Int->Int->Int->Int->Int->Bool
validarFecha diaEntrada mesEntrada anioEntrada diaSalida mesSalida anioSalida = 
    do
        if anioSalida > anioEntrada then
            True
        else
            if anioSalida == anioEntrada then
                if mesSalida > mesEntrada then
                    True 
                else
                    if mesSalida == mesEntrada then
                        if diaSalida > diaEntrada then
                            True 
                        else
                            False
                    else
                        False
            else
                False


{----validarFechaDigitos----
valida que los digitos de las fechas sigan los formatos como que los dias
sean mayores a 0 y menores a 31 o que los meses sean mayores a 0 y menores a 32-}
validarFechaDigitos::Int->Int->Int->Int->Int->Int->Bool
validarFechaDigitos diaEntrada mesEntrada anioEntrada diaSalida mesSalida anioSalida = 
    do
        if diaEntrada > 0 && diaEntrada<32 && diaSalida>0 && diaSalida<32 then
            if mesEntrada>0 && mesEntrada<13 && mesSalida>0 && mesSalida<13 then
                if anioEntrada>2000 && anioSalida>2000 then
                    True 
                else
                    False 
            else 
                False
        else
            False


{----reservacionHabitacion----
Obtiene el tipo de habitacion que el usuario desea para la reservacion-}
reservarHabitacion::String->String->String->String->String->[String]->IO()
reservarHabitacion fechaI fechaS adultos ninos nombre listaHabitaciones = 
    do
        putStrLn "Digite el tipo de habitacion que desea reservar: "
        tipoHabitacion <-getLine
        reservarHabitacionAux fechaI fechaS adultos ninos nombre listaHabitaciones tipoHabitacion
        
{-----reservarHabitacionAux-----
Valida la existencia del tipo de habitacion seleccionadad y la capacidad de huespedes con el tipo de habitacion-}
reservarHabitacionAux::String->String->String->String->String->[String]->String->IO()
reservarHabitacionAux fechaI fechaS adultos ninos nombre listaHabitaciones tipoHabitacion =
    do
        let habitacionExistencia = existeHabitacionCargadaHuespedes listaHabitaciones tipoHabitacion
        let adultosINT = read adultos::Int 
        let ninosINT = read ninos::Int
        let totalHuespedes = adultosINT + ninosINT
        let habitacionExistenciaINT = read habitacionExistencia::Int

        if (habitacionExistenciaINT == 0) then
            errorHabitaciones
        else
            if (totalHuespedes <= habitacionExistenciaINT) then
                guardarReservacion fechaI fechaS adultos ninos nombre tipoHabitacion habitacionExistencia
            else
                errorSumaCantidad

{-----guardarReservacion------
Prepara y guarda los datos de la reservacion en el documento que permite la persistencia de datos-}
guardarReservacion::String->String->String->String->String->String->String->IO()
guardarReservacion fechaI fechaS adultos ninos nombre tipoHabitacion habitacionExistencia = 
    do
        listaCodigosReservacion <- leerArchivo "codigoReservacion.txt"

        cs <- readFile "infoHoteles.txt"
        let listaHotelesMostrar = split ',' cs
        
        listaTarifas <- leerArchivo "Tarifas.txt"
        let strIdentificador = head listaCodigosReservacion
        let identificadorDeReserva = crearIdentificadorReserva strIdentificador
        let intIdentificador = read strIdentificador::Int
        let intIdentificadorNuevo = intIdentificador + 1
        
        let strIdentificadorNuevo = show intIdentificadorNuevo
        fechaActual <- fecha

        resetearArchivo "codigoReservacion.txt" strIdentificadorNuevo
        putStrLn "\n__________________________________COMPROBANTE___________________________________\n"

        let hotel = infoHotel listaHotelesMostrar
        putStrLn hotel

        let precioTotal = obtenerTarifaTipoHabitacion listaTarifas tipoHabitacion

        putStrLn ("\nId Reservacion: " ++ identificadorDeReserva)
        putStrLn ("Nombre del reservante: " ++ nombre)
        putStrLn ("Fecha de reservacion: " ++ fechaActual)
        putStrLn ("Fecha de ingreso: " ++ fechaI)
        putStrLn ("Fecha de salida: " ++ fechaS)
        putStrLn ("Cantidad de adultos: " ++ adultos)
        putStrLn ("Cantidad de ninos: " ++ ninos)
        putStrLn ("Precio total: " ++ precioTotal)
        appendFile "reservaciones.txt" (identificadorDeReserva++ "\n")
        appendFile "reservaciones.txt" (nombre++ "\n")
        appendFile "reservaciones.txt" (fechaActual++ "\n")
        appendFile "reservaciones.txt" (fechaI++ "\n")
        appendFile "reservaciones.txt" (fechaS++ "\n")
        appendFile "reservaciones.txt" (adultos++ "\n")
        appendFile "reservaciones.txt" (ninos ++ "\n")
        appendFile "reservaciones.txt" ("Activa" ++ "\n")
        appendFile "reservaciones.txt" (tipoHabitacion ++ "\n")
        appendFile "reservaciones.txt" (precioTotal ++ "\n")
        terminarReservacion


obtenerTarifaTipoHabitacion::[String]->String->String
obtenerTarifaTipoHabitacion listaTarifas nombre =
    do
        let nombreTipo = head listaTarifas
        let corte = tail listaTarifas
        let tarifa = head corte
        let resto = tail corte

        if (nombre == nombreTipo) then 
            tarifa
        else 
            obtenerTarifaTipoHabitacion resto nombre


existeHabitacionCargadaHuespedes::[String]->String->String
existeHabitacionCargadaHuespedes [] tipoHabitacion = "0" 
existeHabitacionCargadaHuespedes listaHabitaciones tipoHabitacion =
    do 
        let nombreHabitacion = head listaHabitaciones
        let corte = tail (tail listaHabitaciones)
        let maximoHuespedes = head corte
        let resto = tail corte
        
        if (tipoHabitacion == nombreHabitacion) then
            maximoHuespedes
        else 
            existeHabitacionCargadaHuespedes resto tipoHabitacion


{----listaHabitacionesCantMax-----
Prepara una lista con el nombre de las habitaciones cargadas y sus cantidades
maxima de huespedes-}
listaHabitacionesCantMax:: [String]->[String]->[String]
listaHabitacionesCantMax [] res = res
listaHabitacionesCantMax lista res = 
    do
        let nombreHabitacion = head lista
        let corte_1 = tail lista
        let cortePosLista1 = tail corte_1
        let cortePosLista2 = tail cortePosLista1
        let res2 = res++[nombreHabitacion]++[head cortePosLista1]
        listaHabitacionesCantMax cortePosLista2 res2


errorHabitaciones::IO()
errorHabitaciones = 
    do
        putStrLn "\n EL TIPO DE HABITACION SELECCIONADA NO EXISTE. \n"
        menuOpcionesGenerales



errorSumaCantidad::IO()
errorSumaCantidad = 
    do
        putStrLn "\n HUESPEDES SUPERAN EL LIMITE DEL TIPO DE HABITACION \n"
        menuOpcionesGenerales


infoHotel::[String]-> String
infoHotel datosArchivo = 
    do
        let str = "\n_____________________INFORMACION DEL HOTEL_____________________"

        let nombre = head datosArchivo
        let mensajeNombre = "\nNombre: "++nombre
        let resto1 = tail datosArchivo
        let str2 = str ++ mensajeNombre

        let cedula = head resto1
        let mensajeCedula = "\nCedula Juridica: "++cedula 
        let resto2 = tail resto1 
        let str3 = str2 ++ mensajeCedula

        let sitioWeb = head resto2
        let mensajeSitioWeb = "\nSitio Web: "++sitioWeb
        let resto3 = tail resto2
        let str4 = str3 ++ mensajeSitioWeb

        let telefono = head resto3
        let mensajeTelefono = "\nTelefono: "++telefono
        let resto4 = tail resto3
        let str5 = str4 ++ mensajeTelefono

        let pais = head resto4
        let mensajePais = "\nPais: "++pais
        let resto5 = tail resto4
        let str6 = str5 ++ mensajePais

        let provincia = head resto5
        let mensajeProvincia = "\nProvincia: "++provincia
        let str7 = str6 ++ mensajeProvincia

        str7

{-----fecha-----
Obtiene la fecha y hora actual del sistema-}
fecha :: IO String
fecha = 
    do
        zC<-getCurrentTimeZone
        fC<-getCurrentTime
        let (TimeOfDay hora minuto segundo) = localTimeOfDay $ utcToLocalTime zC fC
        let (año,mes,dia) = toGregorian $ utctDay fC
        pAño<-intToString año
        pDia<-intToString dia
        pMes<-intToString mes
        pHora<-intToString hora
        pMinuto<-intToString minuto
        pSegundo<-intToString segundo
        let fechaT = pAño++"/"++pMes++"/"++pDia++" "++pHora++":"++pMinuto++":"++pSegundo
        return fechaT


crearIdentificadorReserva::String->String
crearIdentificadorReserva strID = 
    do
        let identificador = "RE"++strID
        identificador

fechaInvalida ::IO ()
fechaInvalida = 
    do
        putStrLn "ERROR, Digite una fecha valida"
        menuOpcionesGenerales

errorCantHuespedes::IO()
errorCantHuespedes = 
    do
        putStrLn "El numero de huespedes no coincide con la cantidad total"
        menuOpcionesGenerales

terminarReservacion:: IO()
terminarReservacion = 
    do
        putStrLn "\n\t\tSE REALIZO LA RESERVACION CON EXITO\n"
        menuOpcionesGenerales

{-------------------------------Cancelar Reservacion---------------------------------}

{----cancelarReservacion-----
Solicita el identificador de la reservacion que desea cancelar-}
cancelarReservacion::IO()
cancelarReservacion =
    do
        listaReservaciones <-leerArchivo "reservaciones.txt"
        resetearArchivo "reservaciones.txt" ""
        putStrLn "Digite el identificador de la reservacion que desea cancelar: "
        cancelado <- getLine
        cancelarReservacionAux listaReservaciones cancelado

{-----cancelarReservacionAux-------
Recorre una lista generada con el archivo donde se almacenan las reservaciones
cuando encuentra la reservacion generada cambia su estado a Cancelada
En caso de no existir lo notifica y devuelve al menu general-}
cancelarReservacionAux:: [String]->String->IO()
cancelarReservacionAux [] cancelado = menuOpcionesGenerales
cancelarReservacionAux listaReservaciones cancelado = 
    do
        let idReserva = head listaReservaciones
        let corte = tail listaReservaciones
        let nombreReservador = head corte
        let corte2 = tail corte
        let fechaDeReserva = head corte2
        let corte3 = tail corte2
        let fechaEntrada = head corte3
        let corte4 = tail corte3
        let fechaSalida = head corte4
        let corte5 = tail corte4
        let cantAdultos = head corte5
        let corte6 = tail corte5
        let cantNinos = head corte6
        let corte7 = tail corte6
        let estado = head corte7
        let corte8 = tail corte7
        let tipoHabitacion = head corte8
        let corte9 = tail corte8
        let precio = head corte9
        let corte10 = tail corte9
        if (idReserva == cancelado) then
            if (estado == "Activa") then
                cancelacion cancelado idReserva nombreReservador fechaDeReserva fechaEntrada fechaSalida cantAdultos cantNinos "Cancelada" tipoHabitacion precio corte10          
            else
                cancelacionAlready 
        else
            cancelacion cancelado idReserva nombreReservador fechaDeReserva fechaEntrada fechaSalida cantAdultos cantNinos estado tipoHabitacion precio corte10

cancelacionAlready::IO()
cancelacionAlready =
    do
        putStrLn "LA RESERVACION YA ESTA CANCELADA"
        menuOpcionesGenerales

cancelacion::String->String->String->String->String->String->String->String->String->String->String->[String]->IO()
cancelacion cancelado idReserva nombreReservador fechaDeReserva fechaEntrada fechaSalida cantAdultos cantNinos estado tipoHabitacion precio corte10= 
    do
        appendFile "reservaciones.txt" (idReserva++"\n") 
        appendFile "reservaciones.txt" (nombreReservador++"\n")
        appendFile "reservaciones.txt" (fechaDeReserva++"\n")
        appendFile "reservaciones.txt" (fechaEntrada++"\n")
        appendFile "reservaciones.txt" (fechaSalida++"\n")
        appendFile "reservaciones.txt" (cantAdultos++"\n")
        appendFile "reservaciones.txt" (cantNinos++"\n")
        appendFile "reservaciones.txt" (estado++"\n")
        appendFile "reservaciones.txt" (tipoHabitacion++"\n")
        appendFile "reservaciones.txt" (precio++"\n")
        cancelarReservacionAux corte10 cancelado


{-------consultaReservaciones-------
-Funcion principal que se llama del menu administrativo para consultar todas las reservaciones del sistema-}
consultarReservaciones:: IO()
consultarReservaciones = do
    listaReservacion <- leerArchivo "reservaciones.txt"
    consultarReservacionesAux listaReservacion
{----consultarReservacionesAux----
Funcion auxiliar de consultaReservaciones que se encarga de mostrar el identificador, nombre de la persona que reservo, fecha de reserva, fecha de entrada, 
fecha de salida, cantidad de ninos, estado y total-}
consultarReservacionesAux:: [String]->IO()
consultarReservacionesAux [] = menuAdministrativo
consultarReservacionesAux listaReservaciones = do
    let identificador = head listaReservaciones
    let corte = tail listaReservaciones
    let nombreReserva = head corte
    let corte2 = tail corte
    let fechaReserva = head corte2
    let corte3 = tail corte2
    let fechaIngreso = head corte3
    let corte4 = tail corte3
    let fechaSalida = head corte4
    let corte5 = tail corte4
    let cantidadDeAdultos = head corte5
    let corte6 = tail corte5
    let cantidadDeNinos = head corte6
    let corte7 = tail corte6
    let estado = head corte7
    let corte8 = tail corte7
    let nombreTipoHabitacion = head corte8
    let corte9 = tail corte8
    let total= head corte9
    let corte10= tail corte9
    let mensaje = "\nIdentificador: " ++ identificador ++ "\nNombre de la persona que reservo: " ++ nombreReserva ++ "\nFecha de reserva: " ++ fechaReserva ++ "\nFecha de entrada: " ++ fechaIngreso ++ "\nFecha de salida: " ++ fechaSalida ++ "\nCantidad de adultos: " ++ cantidadDeAdultos ++ "\nCantidad de ninos: " ++ cantidadDeNinos ++ "\nEstado: " ++ estado ++ "\nNombre de habitacion: "++nombreTipoHabitacion++"\nTotal: "++total
    putStrLn mensaje
    consultarReservacionesAux corte10


