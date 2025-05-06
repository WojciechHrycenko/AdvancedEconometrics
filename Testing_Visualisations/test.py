import json
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import os

def load_and_preprocess_data():
    """Wczytuje i przygotowuje dane z obsługą błędów"""
    try:
        if not os.path.exists('collisions_sample.json'):
            raise FileNotFoundError("Brak pliku collisions_sample.json")
            
        with open('collisions_sample.json', 'r') as f:
            raw_data = f.read()
            
            # Naprawa formatu JSON
            if not raw_data.strip().startswith('['):
                raw_data = '[' + raw_data + ']'
            raw_data = raw_data.replace('null', 'None')
            
            data = eval(raw_data)
            
        df = pd.DataFrame(data)
        
        # Konwersja kolumn liczbowych
        numeric_cols = ['killed_victims', 'injured_victims', 'distance']
        for col in numeric_cols:
            df[col] = pd.to_numeric(df[col], errors='coerce').fillna(0).astype(int)
            
        # Dodatkowe kolumny
        df['total_victims'] = df['killed_victims'] + df['injured_victims']
        df['collision_date'] = pd.to_datetime(df['collision_date'], errors='coerce')
        df['hour'] = pd.to_datetime(df['collision_time'], format='%H:%M:%S').dt.hour
        
        return df.dropna(subset=['collision_date'])
        
    except Exception as e:
        print(f"BŁĄD: {str(e)}")
        return None

def basic_victim_analysis(df):
    """Analiza podstawowych statystyk ofiar"""
    print("\n" + "="*50)
    print("PODSTAWOWA ANALIZA OFIAR")
    print("="*50)
    
    # Statystyki
    stats = df[['killed_victims', 'injured_victims', 'total_victims']].agg(['sum', 'mean', 'max', 'min'])
    stats.loc['zero_count'] = df[['killed_victims', 'injured_victims', 'total_victims']].eq(0).sum()
    stats.loc['zero_%'] = (stats.loc['zero_count'] / len(df) * 100).round(1)
    
    print(stats.rename(index={
        'sum': 'Łącznie',
        'mean': 'Średnio na zdarzenie',
        'max': 'Maksymalnie',
        'min': 'Minimalnie',
        'zero_count': 'Zdarzenia bez ofiar',
        'zero_%': 'Zdarzenia bez ofiar (%)'
    }))

    # Wykresy
    plt.figure(figsize=(18, 6))
    colors = ['#ff6b6b', '#ff9f43', '#54a0ff']
    
    for i, col in enumerate(['killed_victims', 'injured_victims', 'total_victims']):
        plt.subplot(1, 3, i+1)
        max_val = int(df[col].max())
        sns.histplot(
            df[col], 
            bins=range(0, max_val + 2), 
            discrete=True, 
            color=colors[i], 
            kde=True
        )
        plt.title(f'Rozkład: {col.replace("_", " ").title()}')
        plt.xlabel('Liczba osób')
    
    plt.tight_layout()
    plt.savefig('basic_victims.png', dpi=300)
    plt.show()

def extended_analysis(df):
    """Rozszerzona analiza zmiennych kontekstowych"""
    # Analiza czasowa
    plt.figure(figsize=(15, 6))
    
    plt.subplot(1, 2, 1)
    df['year'] = df['collision_date'].dt.year
    yearly = df.groupby('year').size()
    yearly.plot(kind='line', marker='o', color='navy')
    plt.title('Liczba kolizji w latach')
    
    plt.subplot(1, 2, 2)
    sns.histplot(df['hour'], bins=24, discrete=True, color='#e74c3c')
    plt.title('Rozkład godzinowy kolizji')
    
    plt.tight_layout()
    plt.savefig('time_analysis.png')

def additional_variables_analysis(df):
    """Analiza dodatkowych zmiennych"""
    plt.figure(figsize=(20, 25))
    
    # 1. Hrabstwa
    plt.subplot(3, 2, 1)
    county_counts = df['county_location'].value_counts().head(10)
    sns.barplot(x=county_counts.values, y=county_counts.index, palette='viridis')
    plt.title('Top 10 hrabstw')
    
    # 2. Ciężkość kolizji
    plt.subplot(3, 2, 2)
    severity_order = ['property damage only', 'pain', 'other injury', 'severe injury']
    sns.countplot(y='collision_severity', data=df, order=severity_order, palette='Reds_r')
    plt.title('Ciężkość kolizji')
    
    # 3. Oświetlenie
    plt.subplot(3, 2, 3)
    lighting_counts = df['lighting'].value_counts()
    plt.pie(lighting_counts, labels=lighting_counts.index, autopct='%1.1f%%',
            colors=['#ff9999','#66b3ff','#99ff99','#ffcc99'])
    plt.title('Warunki oświetleniowe')
    
    # 4. Urządzenia kontrolne
    plt.subplot(3, 2, 4)
    control_devices = df['control_device'].value_counts()
    sns.barplot(x=control_devices.values, y=control_devices.index, palette='rocket')
    plt.title('Urządzenia kontrolne')
    
    # 5. Ucieczki
    plt.subplot(3, 2, 5)
    hitrun_counts = df['hit_and_run'].value_counts()
    plt.pie(hitrun_counts, labels=hitrun_counts.index, autopct='%1.1f%%',
            explode=(0.1, 0, 0), shadow=True)
    plt.title('Ucieczki z miejsca zdarzenia')
    
    # 6. Typy patroli
    plt.subplot(3, 2, 6)
    beat_type = df['beat_type'].value_counts().head(8)
    sns.barplot(x=beat_type.values, y=beat_type.index, palette='cubehelix')
    plt.title('Typy patroli')
    
    plt.tight_layout()
    plt.savefig('additional_variables.png')
    plt.show()

if __name__ == "__main__":
    df = load_and_preprocess_data()
    
    if df is not None:
        basic_victim_analysis(df)
        extended_analysis(df)
        additional_variables_analysis(df)
        print("\nAnaliza zakończona! Wygenerowane pliki:")
        print("- basic_victims.png")
        print("- time_analysis.png")
        print("- additional_variables.png")