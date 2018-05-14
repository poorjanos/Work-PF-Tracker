SELECT   TRUNC (SYSDATE, 'mi') idopont,
         a.f_ivk,
         x.f_lean_tip,
         a.f_int_begin,
         a.f_int_end,
         kontakt.basic.get_userid_kiscsoport (a.f_userid) AS csoport,
         UPPER (kontakt.basic.get_userid_login (a.f_userid)) AS login,
         b.f_termcsop,
         CASE
            WHEN (   a.f_alirattipusid BETWEEN 1896 AND 1930
                  OR a.f_alirattipusid BETWEEN 1944 AND 1947
                  OR a.f_alirattipusid IN ('1952', '2027', '2028', '2021'))
            THEN
               kontakt.basic.get_alirattipusid_alirattipus (
                  a.f_alirattipusid
               )
            ELSE
               'Egyéb iraton'
         END
            AS tevekenyseg,
         (a.f_int_end - a.f_int_begin) * 1440 AS cklido
  FROM   afc.t_afc_wflog_lin2 a,
         kontakt.t_lean_alirattipus x,
         kontakt.t_ajanlat_attrib b
 WHERE       TRUNC (a.f_int_begin, 'DDD') = TRUNC (SYSDATE, 'DDD')
         AND (a.f_int_end - a.f_int_begin) * 1440 < 45
         AND (a.f_int_end - a.f_int_begin) * 86400 > 1
         AND afc.afc_wflog_intezkedes (a.f_ivkwfid, a.f_logid) IS NOT NULL
         AND a.f_ivk = b.f_ivk(+)
         AND a.f_alirattipusid = x.f_alirattipusid
         AND UPPER (kontakt.basic.get_userid_login (a.f_userid)) NOT IN
                  ('MARKIB', 'SZERENCSEK')
         AND x.f_lean_tip = 'AL'
         AND b.f_termcsop IS NOT NULL
         AND kontakt.basic.get_userid_kiscsoport (a.f_userid) IS NOT NULL
UNION
SELECT   TRUNC (SYSDATE, 'mi'),
         a.f_ivk,
         x.f_lean_tip,
         a.f_int_begin,
         a.f_int_end,
         kontakt.basic.get_userid_kiscsoport (a.f_userid) AS csoport,
         UPPER (kontakt.basic.get_userid_login (a.f_userid)) AS login,
         'IRAT' AS f_termcsop,
         kontakt.basic.get_alirattipusid_alirattipus (a.f_alirattipusid)
            AS tevekenyseg,
         (a.f_int_end - a.f_int_begin) * 1440 AS time
  FROM   afc.t_afc_wflog_lin2 a, kontakt.t_lean_alirattipus x
 WHERE       TRUNC (a.f_int_begin, 'DDD') = TRUNC (SYSDATE, 'DDD')
         AND (a.f_int_end - a.f_int_begin) * 1440 < 45
         AND (a.f_int_end - a.f_int_begin) * 86400 > 1
         AND afc.afc_wflog_intezkedes (a.f_ivkwfid, a.f_logid) IS NOT NULL
         AND a.f_alirattipusid = x.f_alirattipusid
         AND UPPER (kontakt.basic.get_userid_login (a.f_userid)) NOT IN
                  ('MARKIB', 'SZERENCSEK')
         AND f_lean_tip = 'IL'
         AND kontakt.basic.get_userid_kiscsoport (a.f_userid) IS NOT NULL