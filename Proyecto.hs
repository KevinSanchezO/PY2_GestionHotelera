import System.IO

menu::IO()
menu=
    do
        putStrLn "\nBienvenido al menu principal\n 1. Opciones Administrativas\n 2. Opciones Generales\n 0.Salir\n Digite una opcion: "
        opcion<-getLine
        case opcion of
            "1" -> menuAdministrativo
            "2"->menuOpcionesGenerales
            "0"->return()
            _ -> errorDigito
           
menuAdministrativo :: IO()
menuAdministrativo= 
    do 
        putStrLn "\n*****Opciones Administrativas*****\n 1.Informacion de hotel\n 2.Cargar tipo de habitaciones\n 3.Asignar cantidad de habitaciones por tipo\n 4.Carga de tarifas\n 5.Consultar reservacion\n 6.Consulta de facturas\n 7.Estadisticas de ocupacion\n Digite una opcion: "
        s<-getLine
        case s of
            "1" ->infoHoteles
            "2"->cargarTipoHabitacion
            {-"3"->asignarCantidadHabitacionesTipo
            "4"->cargarTarifas
            "5"->consultarReservacion
            "6"->consultaFacturas
            "7"->estadisticasOcupacion-}
            _ -> errorDigito

errorDigito::IO()
errorDigito=
    do
        putStr "\ESC[2J"
        putStrLn "Error!!, Digite una opcion valida"
        menu

menuOpcionesGenerales::IO()
menuOpcionesGenerales=
    do
        putStrLn "\n******Opciones Generales*****\n 1.Reservacion\n 2.Cancelar Reservacion\n 3.Facturar Reservacion\n 4.Salir\n Digite una opcion: "
        d<-getLine
        case d of
            {-"1"->reservacion
            "2"->cancelarReservacion
            "3"->facturarReservacion
            "4"->menu-}
            _ ->errorDigito

infoHoteles::IO()
infoHoteles =
    do
        putStrLn "\nInformacion del hotel\nNombre   Cedula Juridica   Sitio Web   Telefono   Pais   Provincia"
        cs <- readFile "infoHoteles.txt"
        putStrLn cs

cargarTipoHabitacion:: IO()
cargarTipoHabitacion =
    do 
        putStrLn "\nIngrese la ruta del archivo con los tipos de habitaciones: "
        ruta <- getLine

        cs <- readFile ruta
        putStrLn cs