#!/usr/bin/env bash
# =============================================================================
# Descarga de devengado mensual desde la API de Presupuesto Abierto.
#
# Basado en get_2024.sh, pero en lugar de traer sólo el programa 44 de la
# Agencia de Promoción y Financiamiento de Actividades de Ciencia, Tecnología
# e Innovación, trae TODO el devengado clasificado por finalidad/función:
#
#   * Finalidad 3 (Servicios Sociales), Función 4 -> Educación y Cultura
#   * Finalidad 3 (Servicios Sociales), Función 5 -> Ciencia, Tecnología e Innovación
#
# Al filtrar por función (y no por programa) se incluyen automáticamente todos
# los organismos descentralizados: CONICET, INTA, INTI, CNEA, CONAE, ANLIS,
# universidades nacionales, etc.
# =============================================================================
set -euo pipefail

API_URL='https://www.presupuestoabierto.gob.ar/api/v1/credito?format=json'
TOKEN='cbbd85c1-1986-4491-a5f6-8de8f4deb733'
OUT='devengado_ciencia_educacion.json'

# Columnas: desagregación mensual (impacto_presupuestario_anio/mes) + función.
COLUMNS='["impacto_presupuestario_anio","impacto_presupuestario_mes","ejercicio_presupuestario","finalidad_id","finalidad_desc","funcion_id","funcion_desc","credito_devengado"]'

# La API acepta ejercicios >= 1995 (o el literal "vigente").
YEARS=(1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 \
       2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 \
       2023 2024 2025 2026)

TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

for y in "${YEARS[@]}"; do
  echo "Descargando ejercicio $y ..." >&2
  curl -s -X POST "$API_URL" \
    -H "Authorization: $TOKEN" \
    -H 'Content-Type: application/json' \
    -d "{
      \"columns\": $COLUMNS,
      \"ejercicios\": [$y],
      \"filters\": [
        {
          \"column\": \"finalidad_id\",
          \"value\": \"3\",
          \"operator\": \"equal\"
        }
      ]
    }" > "$TMPDIR/$y.json"
done

# Combinar los JSON de cada ejercicio en un único arreglo.
jq -s 'add' "$TMPDIR"/*.json > "$OUT"
echo "Escrito $OUT" >&2
