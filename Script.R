require(R6)
require(stringr)
require(redux)
require(yaml)

Script <- R6Class("Script",
    private = list(
        name    = NULL,
        run.dir = Sys.getenv(x = "PLOTTER_HOME", unset = "/tmp"),
        
        getPidFile = function() {
          return (paste0(private$run.dir, "/", private$name, ".pid"))
        },
        
        getPid = function() {
          if (redux::redis_available()) {
            r <- redux::hiredis()
            pid.str <- r$GET(private$name)  # NULL si no existe en redis
            if (!is.null(pid.str)) {
              return (as.integer(pid.str))
            } else {
              return (NULL)
            }
          } else {
            pid.file <- private$getPidFile()
            if (file.exists(pid.file)) {
              return (as.integer(readLines(con = pid.file, n = 1, warn = FALSE)))
            } else {
              return (NULL)
            }
          }
        },
        
        isRunning = function() {
          return (! is.null(private$getPid()))
        }
    ),
    public = list(
        initialize = function(name = NULL) {
          # Verificar que se haya especificado un nombre para el script
          if (is.null(name) || (length(name) == 0)) {
            stop("Debe indicar un nombre para el script")
          }
          # Asginar nombre a la variable privada name
          private$name <- name          
          # Se extrae la "/" al final de la variable privada run.dir
          if (stringr::str_sub(private$run.dir, -1) == "/") {
            private$run.dir = stringr::str_sub(private$run.dir, 1, -2)
          }
        },
        
        start = function() {
          pid <- private$getPid()
          if (is.null(pid)) {
            pid <- Sys.getpid()
            if (redux::redis_available()) {
              r <- redux::hiredis()
              r$SET(private$name, pid)
            } else {
              pid.file <- private$getPidFile()
              writeLines(text = as.character(pid), con = pid.file)
            }
            logger::log_info(paste0("Iniciando script ", private$name, " con PID ", pid))
          } else {
            logger::log_error(paste0("El script actualmente esta corriendo con el PID ", pid))
          }
        },
        
        assertNotRunning = function(abort.execution = TRUE) {
          if (private$isRunning()) {
            logger::log_error(
              paste0("Se esperaba que el script ", private$name, " no esté ejecutándose, ",
                     "pero actualmente está corriendo con PID ", private$getPid()) )
            if (abort.execution) base::stop("Abortando ejecucion")
          }
        },
        
        stop = function() {
          pid <- private$getPid()
          if (! is.null(pid)) {
            if (redux::redis_available()) {
              r <- redux::hiredis()
              r$DEL(private$name)
            } else {
              pid.file <- private$getPidFile()
              file.remove(pid.file)
            }
            logger::log_info(paste0("Finalizando script ", private$name))
          } else {
            logger::log_error("El script no estaba actualmente corriendo")
          }
        }
    )
)
