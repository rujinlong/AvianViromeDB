#!/usr/bin/env python3

import os
import subprocess
import pandas as pd
import sqlite3


def create_vOTU_fasta(fasta_genomes_input, fasta_genomes_output, conn):
    tmp_votu_id = "tmp_votu_id.csv"
    df_votu_id = pd.read_sql_query(f"SELECT contig_repid FROM contig_annotation_filtered_vOTU", conn)
    df_votu_id.to_csv(tmp_votu_id, header=False, index=False)

    # using seqkit to extract selected votus
    cmd = f"seqkit grep -f {tmp_votu_id} {fasta_genomes_input} -o {fasta_genomes_output}"
    subprocess.run(cmd, shell=True, check=True)

    # Remove temporary file
    os.remove(tmp_votu_id)
    return(df_votu_id)


def create_vOTU_protein(df_votu_ids, fasta_protin_input, fasta_protein_output):
    tmp_prot_id = "tmp_prot_ids.csv"
    tmp_protid_all = "tmp_protid_all.csv"
    df_votu_protein_ids = df_votu_ids.copy()
    df_votu_protein_ids['protein_id_prefix'] = df_votu_ids['contig_repid'] + '_'

    cmd = f"seqkit fx2tab -n {fasta_protin_input} | cut -d ' ' -f1 > {tmp_protid_all}"
    subprocess.run(cmd, shell=True, check=True)
    df_protid_all = pd.read_csv(tmp_protid_all, header=None, names=['protein_id'])
    df_protid_all['protein_id_prefix'] = df_protid_all['protein_id'].str.replace(r'_[0-9]*$', '_', regex=True)
    df_protid_sel = pd.merge(df_protid_all, df_votu_protein_ids, on='protein_id_prefix', how='inner')
    df_protid_sel[['protein_id']].to_csv(tmp_prot_id, header=False, index=False)

    cmd = f"seqkit grep -f {tmp_prot_id} {fasta_protin_input} -o {fasta_protein_output}"
    subprocess.run(cmd, shell=True, check=True)
    os.remove(tmp_prot_id)
    os.remove(tmp_protid_all)


def create_vOTU_annotation(conn_raw, conn_pub, output):
    df_vOTU_annotation = pd.read_sql_query(f"SELECT * FROM contig_annotation_filtered_vOTU", conn_raw)
    df_vOTU_annotation = df_vOTU_annotation.sort_values('vOTU_id')
    df_vOTU_annotation.to_csv(output, index=False, sep="\t")

    # Write the vOTU annotation data to the public database
    df_vOTU_annotation.to_sql('vOTU_annotation', conn_pub, if_exists='replace', index=False)
    return(df_vOTU_annotation)


def filter_by_vOTU(df, df_vOTU_annotation, conn_raw):
    df_ctgid2repid = pd.read_sql_query(f"SELECT contig_id, contig_repid, vOTU_id FROM contig_annotation", conn_raw)
    df_sel = pd.merge(df, df_ctgid2repid, on='contig_id', how='inner')
    df_sel = df_sel.drop('contig_id', axis=1)
    df_sel = df_sel[['contig_repid', 'vOTU_id'] + [col for col in df_sel.columns if col not in ['contig_repid', 'vOTU_id']]]
    df_sel = df_sel.sort_values('vOTU_id')
    df_sel_vOTU = pd.merge(df_sel, df_vOTU_annotation[["contig_repid"]], on='contig_repid', how='inner')
    return(df_sel_vOTU)


def create_vOTU_hosts(conn_raw, conn_pub, df_vOTU_annotation, output):
    df_prokaryotic_hosts = pd.read_sql_query(f"SELECT * FROM host_filtered", conn_raw)
    df_prokyriotic_hosts_sel_vOTU = filter_by_vOTU(df_prokaryotic_hosts, df_vOTU_annotation, conn_raw)
    # output
    df_prokyriotic_hosts_sel_vOTU.to_csv(output, index=False, sep="\t")
    df_prokyriotic_hosts_sel_vOTU.to_sql('virus_prokyriotic_hosts', conn_pub, if_exists='replace', index=False)
    return(df_prokyriotic_hosts_sel_vOTU)


def create_vOTU_amg(conn_raw, conn_pub, df_vOTU_annotation, output, software):
    """
    software: 'dramv' or 'vibrant'
    """
    df_amg = pd.read_sql_query(f"SELECT * FROM amg_{software}_filtered", conn_raw)
    df_amg_sel_vOTU = filter_by_vOTU(df_amg, df_vOTU_annotation, conn_raw)
    # output
    df_amg_sel_vOTU.to_csv(output, index=False, sep="\t")
    df_amg_sel_vOTU.to_sql(f"amg_{software}", conn_pub, if_exists='replace', index=False)
    return(df_amg_sel_vOTU)


def main():
    wd = os.getcwd()
    path_data = os.path.join(wd, "data/00-raw/d99-db_publish")
    path_target = os.path.join(wd, "data/99-db_publish")
    conn_raw = sqlite3.connect("pc028e3.sqlite")
    conn_pub = sqlite3.connect("AvianViromeDB.sqlite")
    fasta_genomes = os.path.join(path_data, "ccflt.fasta.gz")
    fasta_protin_input = os.path.join(path_data, "proteins.faa")
    fasta_protein_output = os.path.join(path_target, "vOTU_proteins.faa.gz")

    df_votu_ids = create_vOTU_fasta(fasta_genomes, f"{path_target}/vOTUs.fasta.gz", conn_raw)
    df_vOTU_annotation = create_vOTU_annotation(conn_raw, conn_pub, f"{path_target}/vOTU_annotation.tsv.gz")
    df_prokyriotic_hosts = create_vOTU_hosts(conn_raw, conn_pub, df_vOTU_annotation, f"{path_target}/vOTU_prokyriotic_hosts.tsv.gz")

    df_amg_dramv = create_vOTU_amg(conn_raw, conn_pub, df_vOTU_annotation, f"{path_target}/AMG_DRAMv.tsv.gz", "dramv")
    df_amg_vibrant = create_vOTU_amg(conn_raw, conn_pub, df_vOTU_annotation, f"{path_target}/AMG_VIBRANT.tsv.gz", "vibrant")
    os.chdir(path_target)
    files_tar = ["AMG_DRAMv.tsv.gz", "AMG_VIBRANT.tsv.gz"]
    command = ['tar', '-czf', f'{path_target}/gene_annotations.tar.gz'] + files_tar
    subprocess.run(command, check=True, text=True)

    # Remove the files after creating the tar archive
    for file in files_tar:
        os.remove(file)

    os.chdir(wd)

    create_vOTU_protein(df_votu_ids, fasta_protin_input, fasta_protein_output)


if __name__ == '__main__':
    main()