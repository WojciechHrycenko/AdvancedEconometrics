import sqlite3
import json
import os
import random

def export_table_samples(db_path, output_dir='json_samples', sample_size=10000):
    """
    Eksportuje próbki danych z każdej tabeli w bazie SQLite do osobnych plików JSON
    
    :param db_path: ścieżka do pliku bazy danych SQLite
    :param output_dir: katalog wyjściowy dla plików JSON
    :param sample_size: liczba rekordów do pobrania z każdej tabeli
    """
    # Połączenie z bazą danych
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()
    
    # Utwórz katalog wyjściowy, jeśli nie istnieje
    os.makedirs(output_dir, exist_ok=True)
    
    # Pobierz listę wszystkich tabel w bazie danych
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table';")
    tables = [table[0] for table in cursor.fetchall()]
    
    for table_name in tables:
        print(f"\nPrzetwarzanie tabeli: {table_name}")
        
        try:
            # Pobierz całkowitą liczbę rekordów w tabeli
            cursor.execute(f"SELECT COUNT(*) FROM {table_name};")
            total_records = cursor.fetchone()[0]
            
            # Jeśli tabela ma mniej rekordów niż sample_size, bierzemy wszystkie
            actual_sample_size = min(sample_size, total_records)
            
            # Pobierz próbkę danych (losowe rekordy)
            if total_records <= sample_size:
                # Jeśli tabela jest mała, bierzemy wszystkie rekordy
                cursor.execute(f"SELECT * FROM {table_name};")
            else:
                # Dla dużych tabel pobieramy losowe rekordy
                cursor.execute(f"SELECT * FROM {table_name} ORDER BY RANDOM() LIMIT {actual_sample_size};")
            
            rows = cursor.fetchall()
            
            # Pobierz nazwy kolumn
            column_names = [description[0] for description in cursor.description]
            
            # Konwertuj dane do listy słowników
            data = [dict(zip(column_names, row)) for row in rows]
            
            # Zapisz do pliku JSON
            output_path = os.path.join(output_dir, f"{table_name}_sample.json")
            with open(output_path, 'w', encoding='utf-8') as f:
                json.dump(data, f, indent=4, ensure_ascii=False)
            
            print(f"Zapisano próbkę {len(data)}/{total_records} rekordów do {output_path}")
            
        except sqlite3.Error as e:
            print(f"Błąd podczas przetwarzania tabeli {table_name}: {e}")
    
    # Zamknij połączenie
    conn.close()
    print("\nEksport próbek zakończony!")

# Użycie funkcji
export_table_samples('switrs.sqlite', sample_size=10000)