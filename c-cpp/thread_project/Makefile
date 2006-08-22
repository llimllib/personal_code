CC      := g++
CFLAGS  := -Wall -lpthread

all: ex_mutex ex_race_cond ex_petersons ex_semaphore ex_buffer ex_p_v ex_sleep_buffer

ex_p_v: p_v/p_v_mutex.cpp
	${CC} ${CFLAGS} -o ex_p_v_mutex p_v/p_v_mutex.cpp

ex_mutex: mutex/mutex.cpp
	${CC} ${CFLAGS} -o ex_mutex mutex/mutex.cpp

ex_race_cond: race_cond/race.cpp
	${CC} ${CFLAGS} -o ex_race_cond race_cond/race.cpp

ex_petersons: petersons/petersons.cpp
	${CC} ${CFLAGS} -o ex_petersons petersons/petersons.cpp

ex_semaphore: semaphore/semaphore.cpp
	${CC} ${CFLAGS} -o ex_semaphore semaphore/semaphore.cpp

ex_buffer: bound_buffer/bound_buffer.cpp
	${CC} ${CFLAGS} -o ex_buffer bound_buffer/bound_buffer.cpp

ex_sleep_buffer: bound_buffer/sleep_buffer.cpp
	${CC} ${CFLAGS} -o ex_sleep_buffer bound_buffer/sleep_buffer.cpp
