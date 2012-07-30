/*
 * Module:  egtm_worker -- EGTM NIF library
 * Created: 05-APR-2012 20:11
 * Author:  tmr
 *
 * Copyright 2012 Tomas Morstein, IDEA Systems.
 *
 * This program is free software: you can redistribute
 * it and/or modify it under the terms of the GNU Affero
 * General Public License as published by the Free Software
 * Foundation, either version 3 of the License,
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will
 * be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Affero General
 * Public License for more details.
 *
 * You should have received a copy of the GNU Affero
 * General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 */

#include <termios.h>
#include <stdbool.h>
//#include <stdio.h>

#include "erl_nif.h"
#include "gtmxc_types.h"

#define EGTM$BUFLEN        512
#define EGTM$BUFLENBIG   65536
#define EGTM$LOCKNAM     "EGTM$MUTEX"

#define LOCK(critical_section) { \
  enif_mutex_lock (m_Lock); \
  { critical_section } \
  enif_mutex_unlock (m_Lock); \
 }

#define NIFARGS \
  ErlNifEnv * env, int argc, const ERL_NIF_TERM argv []
#define NIF(name) \
  ERL_NIF_TERM (name) (NIFARGS)

ERL_NIF_TERM    c_AtomOK;
ERL_NIF_TERM    c_AtomError;

ErlNifMutex   * m_Lock;

struct termios m_StderrOrig, m_StdinOrig, m_StdoutOrig;

int check_status (gtm_status_t status, char msg []) {

  if (status != 0) {
    gtm_zstatus (msg, EGTM$BUFLEN);
    
    return -10;
  }

  return 0;
}

static int load_internal (ErlNifEnv * env, void ** priv, void ** old_priv, ERL_NIF_TERM load_info, bool upgrade) {

  c_AtomOK    = enif_make_atom (env, "ok");
  c_AtomError = enif_make_atom (env, "error");
  m_Lock      = enif_mutex_create (EGTM$LOCKNAM);

  // XXX: not so correct in the case of upgrade!!
  tcgetattr (0, &m_StdinOrig);
  tcgetattr (1, &m_StdoutOrig);
  tcgetattr (2, &m_StderrOrig);

  /*
  * priv = malloc (sizeof (int));
  if (old_priv == NULL)
    ** (int **) priv = 0;
  else
    ** (int **) priv = (** (int **) old_priv) +1;
  fprintf (stderr, "PRIV=%d\n", (int) ** (int **) priv);
  */

  if (!upgrade) {
    char emsg [EGTM$BUFLEN];
    gtm_status_t status;
  
    status = gtm_init ();
    check_status (status, emsg); // XXX: check status

    // XXX: do we want it to call only on init or also on upgrade?
    status = gtm_ci ("m_init");
    check_status (status, emsg); // XXX: check status
  }

  return 0;
}

static int load (ErlNifEnv * env, void ** priv, ERL_NIF_TERM load_info) {

  //fprintf (stderr, "LOAD\n");
  return load_internal (env, priv, NULL, load_info, false);
}

/*
static int upgrade (ErlNifEnv * env, void ** priv, void ** old_priv, ERL_NIF_TERM load_info) {

  //fprintf (stderr, "UPGRADE\n");
  return load_internal (env, priv, old_priv, load_info, true);
}

static int reload (ErlNifEnv * env, void ** priv, ERL_NIF_TERM load_info) {

  //fprintf (stderr, "RELOAD\n");
  return 0;
}
*/

static void unload (ErlNifEnv * env, void * priv) {

  //fprintf (stderr, "UNLOAD=%d\n", * (int *) priv);

  enif_mutex_destroy (m_Lock);
  gtm_status_t status = gtm_exit ();

  // XXX: check status
  char emsg [EGTM$BUFLEN];
  check_status (status, emsg);

  tcsetattr (0, 0, &m_StdinOrig);
  tcsetattr (1, 0, &m_StdoutOrig);
  tcsetattr (2, 0, &m_StderrOrig);
}

NIF (m_horo) {

  char val [64]; gtm_status_t status;
  LOCK(status = gtm_ci ("m_horo", val);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return enif_make_tuple2 (env, c_AtomOK,
        enif_make_string (env, val, ERL_NIF_LATIN1));
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_job) {

  char val [64]; gtm_status_t status;
  LOCK(status = gtm_ci ("m_job", val);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return enif_make_tuple2 (env, c_AtomOK,
        enif_make_string (env, val, ERL_NIF_LATIN1));
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_zver) {

  char val [128];
  gtm_status_t status;
  LOCK(status = gtm_ci ("m_zver", val);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return enif_make_tuple2 (env, c_AtomOK,
        enif_make_string (env, val, ERL_NIF_LATIN1));
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_get) {

  if (argc != 1) return enif_make_badarg (env);

  char key [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], key, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  char val [EGTM$BUFLENBIG];
  gtm_status_t status;
  LOCK(status = gtm_ci ("m_get", val, key);)

  char emsg [EGTM$BUFLENBIG];
  if (check_status (status, emsg) == 0)
    return enif_make_tuple2 (env, c_AtomOK,
        enif_make_string (env, val, ERL_NIF_LATIN1));
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_getp) {

  if (argc != 3) return enif_make_badarg (env);

  char key [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], key, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  int piece;
  if (enif_get_int (env, argv [1], &piece) < 0)
    return enif_make_badarg (env);

  char delim [EGTM$BUFLEN];
  if (enif_get_string (env, argv [2], delim, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  char val [EGTM$BUFLENBIG];
  gtm_status_t status;
  LOCK(status = gtm_ci ("m_getp", val, key, piece, delim);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return enif_make_tuple2 (env, c_AtomOK,
        enif_make_string (env, val, ERL_NIF_LATIN1));
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_set) {

  if (argc != 2) return enif_make_badarg (env);

  char key [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], key, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  char val [EGTM$BUFLENBIG];
  if (enif_get_string (env, argv [1], val, EGTM$BUFLENBIG, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  gtm_status_t status;
  LOCK(status = gtm_ci ("m_set", key, val);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return c_AtomOK;
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_setp) {

  if (argc != 4) return enif_make_badarg (env);

  char key [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], key, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  int piece;
  if (enif_get_int (env, argv [1], &piece) < 0)
    return enif_make_badarg (env);

  char delim [EGTM$BUFLEN];
  if (enif_get_string (env, argv [2], delim, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  char val [EGTM$BUFLENBIG];
  if (enif_get_string (env, argv [3], val, EGTM$BUFLENBIG, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  gtm_status_t status;
  LOCK(status = gtm_ci ("m_setp", key, piece, delim, val);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return c_AtomOK;
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_order) {

  if (argc != 2) return enif_make_badarg (env);

  char gbl [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], gbl, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  char key [EGTM$BUFLEN];
  int dir;
  if (enif_get_int (env, argv [1], &dir) < 0)
    return enif_make_badarg (env);

  gtm_status_t status;
  LOCK(status = gtm_ci ("m_order", key, gbl, dir);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return enif_make_tuple2 (env, c_AtomOK,
        enif_make_string (env, key, ERL_NIF_LATIN1));
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_fast_order) {

  if (argc != 2) return enif_make_badarg (env);

  char gbl [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], gbl, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  int dir;
  if (enif_get_int (env, argv [1], &dir) < 0)
    return enif_make_badarg (env);

  char key [EGTM$BUFLEN];
  gtm_status_t status;
  LOCK(status = gtm_ci ("m_fast_order", key, gbl, dir);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return enif_make_tuple2 (env, c_AtomOK,
        enif_make_string (env, key, ERL_NIF_LATIN1));
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_kill) {

  if (argc != 1) return enif_make_badarg (env);

  char gvn [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], gvn, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  gtm_status_t status;
  LOCK(status = gtm_ci ("m_kill", gvn);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return c_AtomOK;
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_zkill) {

  if (argc != 1) return enif_make_badarg (env);

  char gvn [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], gvn, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  gtm_status_t status;
  LOCK(status = gtm_ci ("m_zkill", gvn);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return c_AtomOK;
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_do) {

  if (argc != 1) return enif_make_badarg (env);

  char cmd [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], cmd, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  gtm_status_t status;
  LOCK(status = gtm_ci ("m_do", cmd);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return c_AtomOK;
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_call) {

  if (argc != 1) return enif_make_badarg (env);

  char cmd [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], cmd, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  char res [EGTM$BUFLENBIG]; res[0]='\0';
  gtm_status_t status;
  LOCK(status = gtm_ci ("m_call", res, cmd);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return enif_make_tuple2 (env, c_AtomOK,
        enif_make_string (env, res, ERL_NIF_LATIN1));
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_merge) {

  if (argc != 2) return enif_make_badarg (env);

  char gvn1 [EGTM$BUFLEN]; char gvn2 [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], gvn1, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);
  if (enif_get_string (env, argv [1], gvn2, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  gtm_status_t status;
  LOCK(status = gtm_ci ("m_merge", gvn1, gvn2);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return c_AtomOK;
  else
    return enif_make_tuple2 (env, c_AtomError,
      enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_xecute) {

  if (argc != 1) return enif_make_badarg (env);

  char cmd [EGTM$BUFLENBIG];
  if (enif_get_string (env, argv [0], cmd, EGTM$BUFLENBIG, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  gtm_status_t status;
  LOCK(status = gtm_ci ("m_xecute", cmd);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return c_AtomOK;
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_tstart) {

  if (argc != 1) return enif_make_badarg (env);

  char opts [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], opts, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  gtm_status_t status;
  LOCK(status = gtm_ci ("m_tstart", opts);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return c_AtomOK;
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_tcommit) {

  if (argc != 0) return enif_make_badarg (env);

  gtm_status_t status;
  LOCK(status = gtm_ci ("m_tcommit");)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return c_AtomOK;
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_trollback) {

  if (argc != 0) return enif_make_badarg (env);

  gtm_status_t status;
  LOCK(status = gtm_ci ("m_trollback");)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return c_AtomOK;
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_lock) {

  if (argc != 1) return enif_make_badarg (env);

  char gvn [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], gvn, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  gtm_status_t status;
  LOCK(status = gtm_ci ("m_lock", gvn);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return c_AtomOK;
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_unlock) {

  if (argc != 1) return enif_make_badarg (env);

  char gvn [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], gvn, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  gtm_status_t status;
  LOCK(status = gtm_ci ("m_unlock", gvn);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return c_AtomOK;
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

NIF (m_data) {

  if (argc != 1) return enif_make_badarg (env);

  char gvn [EGTM$BUFLEN];
  if (enif_get_string (env, argv [0], gvn, EGTM$BUFLEN, ERL_NIF_LATIN1) < 0)
    return enif_make_badarg (env);

  int ret; gtm_status_t status;
  LOCK(status = gtm_ci ("m_data", &ret, gvn);)

  char emsg [EGTM$BUFLEN];
  if (check_status (status, emsg) == 0)
    return enif_make_tuple2 (env, c_AtomOK,
        enif_make_int (env, ret));
  else
    return enif_make_tuple2 (env, c_AtomError,
        enif_make_string (env, emsg, ERL_NIF_LATIN1));
}

static ErlNifFunc nif_funcs [] = {
    {"m_set",        2, m_set},
    {"m_setp",       4, m_setp},
    {"m_get",        1, m_get},
    {"m_getp",       3, m_getp},
    {"m_order",      2, m_order},
    {"m_fast_order", 2, m_fast_order},
    {"m_kill",       1, m_kill},
    {"m_zkill",      1, m_zkill},
    {"m_do",         1, m_do},
    {"m_call",       1, m_call},
    {"m_merge",      2, m_merge},
    {"m_xecute",     1, m_xecute},
    {"m_tstart",     1, m_tstart},
    {"m_tcommit",    0, m_tcommit},
    {"m_trollback",  0, m_trollback},
    {"m_lock",       1, m_lock},
    {"m_unlock",     1, m_unlock},
    {"m_data",       1, m_data},
    {"m_horo",       0, m_horo},
    {"m_zver",       0, m_zver},
    {"m_job",        0, m_job}
};

//ERL_NIF_INIT (egtm_worker, nif_funcs, &load, &reload, &upgrade, &unload);
ERL_NIF_INIT (egtm_worker, nif_funcs, &load, NULL, NULL, &unload);

// vim: fdm=syntax:fdn=1:tw=74:ts=2:syn=c
