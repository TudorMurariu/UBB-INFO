Esto es GNU CLISP, una implementación de Common Lisp.


¿ Qué es LISP ?
---------------

LISP es un lenguaje de programación inventado por J. McCarthy en
1959. Aunque ha habido muchos dialectos de él, actualmente se ha
estandarizado y difundido ampliamente gracias al estandar industrial
COMMON LISP. Hay aplicaciones en los dominios del procesamiento del
conocimiento simbólico (IA), cálculo numérico (MACLISP generaba código
tan bueno como el de FORTRAN), y en programas ampliamente utilizados
como editores (EMACS) y CAD (AUTOCAD). Si lo desea, puede consultar la
introducción al lenguaje LISP:

  Sheila Hughes: Lisp. Pitman Publishing Limited, London 1986.
  107 pages.

Después de un rato, necesitará el texto estandar que contiene la
definición del lenguaje:

Guy L. Steele Jr.: Common Lisp - The Language. Digital Press.
  1. edition 1984, 465 pages.
  2. edition 1990, 1032 pages.

Este libro está disponible en formato HTML via FTP en:
  ftp.cs.cmu.edu:/user/ai/lang/lisp/doc/cltl/cltl_ht.tgz

y puede consultarse a través de WWW en:

  http://www.cs.cmu.edu:8001/Web/Groups/AI/html/cltl/cltl2.html o
  http://www.cs.cmu.edu:8001/afs/cs/project/ai-repository/ai/html/cltl/cltl2.html .

Nota para los expertos: Este texto estandar se ha convertido en un
estándar ANSI, que puede obtenerse <<<exceptionally>>> sin cargo alguno en:

  http://www.lisp.org/HyperSpec/FrontMatter/

LISP se ejecuta en un entorno interactivo. Usted introduce formas, que
serán evaluadas de inmediato. Por lo tanto, puede inspeccionar
variables, invocar funciones con unos argumentos concretos o definir
sus propias funciones.


Contenidos:
-----------

Consta de los siguientes ficheros:

      lispinit.mem       imagen de memoria necesaria para la inicialización
      clisp.1            manual en formato `man' de Unix
      clisp.man          manual
      clisp.html         manual en format HTML
      impnotes.html      notas de la implementación
      LISP-tutorial.txt  tutorial de LISP para aprendices
      CLOS-guide.txt     breve guía de CLOS
      editors.txt        <<<some words about text editors for Lisp>>>
      README             este texto
      SUMMARY            pequeña descripción de CLISP
      ANNOUNCE           declaración
      NEWS               lista de modificaciones desde la última versión
      COPYRIGHT          derechos de autor
      GNU-GPL            licencia de software libre
      config.lisp        configuración dependiente del lugar

y - cuando le apetezca, si le gusta leer código fuente -

      *.lisp             el código fuente de lispinit.mem
      *.fas              los mismos ficheros, una vez compilados


Instalación:
------------

Cambie las cadenas en SRC/CONFIG.LISP, empleando para ello un editor de
textos.
Luego ejecute

         lisp.exe -M lispinit.mem

Cuando aparezca el inductor de comandos

      > _

teclee

      (without-package-lock ()
        (compile-file "src/config.lisp")
        (load "src/config.fas"))

y luego

        (saveinitmem)

para sobreescribir el fichero LISPINIT.MEM con su configuración. A
continuación

        (exit)

Luego cree un directorio, y ponga en él el ejecutable con la imagen de
memoria.
Suponiendo D:\LIB\LISP :

   mkdir d:\lib\lisp
   copy lisp.exe d:\lib\lisp
   copy lispinit.mem d:\lib\lisp

Y cree un fichero de ejecución por lotes que ejecute lisp:

   copy con c:\bat\clisp.bat
   d:\lib\lisp\lisp.exe -M d:\lib\lisp\lispinit.mem -B d:\lib\lisp\ %1 %2 %3 %4 %5 %6 %7 %8 %9
   [Ctrl-Z]


Cuando encuentre problemas:
---------------------------

Después de un error, se encontrará en el depurador:

     1. Break> _

En él, usted puede evaluar formas como siempre. Más aún:

     Help
               invoca la ayuda
     Abort     o
     Unwind
               retrocede hasta el bucle de entrada más reciente
     Backtrace
               muestra los contenidos de la pila, útil para la depuración

Y puede consultar el valor de las variables de las funciones donde se
produjo el error.

envíe una descripción del error y una descripción de cómo reproducir
el error a los autores o al "mantenedor". Por favor, acompañe su mensaje
de la versión de CLISP que puede obtener invocando la función
(lisp-implementation-version).


Código fuente:
--------------

El código fuente de CLISP está disponible en
     ftp://clisp.cons.org/pub/lisp/clisp/source/clispsrc*


Lista de correo:
----------------

<<<There are three mailing lists for users of CLISP. You find subscription
information and archives on the homepage http://clisp.cons.org/.>>>


Agradecimientos:
----------------

Estamos muy agradecidos a
  * Guy L. Steele y otros muchos por la especificación de Common Lisp.


Autores:
--------

        Bruno Haible
        Michael Stoll

Email: clisp-list@lists.sourceforge.net

"Mantenedor":
-------------

        Sam Steingold

Email: clisp-list@lists.sourceforge.net
