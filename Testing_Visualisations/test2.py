import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

def analyze_additional_variables(df):
    plt.figure(figsize=(20, 25))
    
    # 1. COUNTY_LOCATION - Rozkład geograficzny
    plt.subplot(3, 2, 1)
    county_counts = df['county_location'].value_counts().head(10)
    sns.barplot(x=county_counts.values, y=county_counts.index, palette='viridis')
    plt.title('Top 10 hrabstw z największą liczbą kolizji')
    plt.xlabel('Liczba zdarzeń')
    
    # 2. COLLISION_SEVERITY - Ciężkość kolizji
    plt.subplot(3, 2, 2)
    severity_order = ['property damage only', 'pain', 'other injury', 'severe injury']
    sns.countplot(y='collision_severity', data=df, order=severity_order, palette='Reds_r')
    plt.title('Rozkład ciężkości kolizji')
    
    # 3. LIGHTING - Oświetlenie
    plt.subplot(3, 2, 3)
    lighting_counts = df['lighting'].value_counts()
    plt.pie(lighting_counts, labels=lighting_counts.index, autopct='%1.1f%%',
            colors=['#ff9999','#66b3ff','#99ff99','#ffcc99'])
    plt.title('Warunki oświetleniowe')
    
    # 4. CONTROL_DEVICE - Urządzenia kontrolne
    plt.subplot(3, 2, 4)
    control_devices = df['control_device'].value_counts()
    sns.barplot(x=control_devices.values, y=control_devices.index, palette='rocket')
    plt.title('Obecność urządzeń kontrolnych')
    plt.xlabel('Liczba zdarzeń')
    
    # 5. HIT_AND_RUN - Ucieczki z miejsca zdarzenia
    plt.subplot(3, 2, 5)
    hitrun_counts = df['hit_and_run'].value_counts()
    plt.pie(hitrun_counts, labels=hitrun_counts.index, autopct='%1.1f%%',
            explode=(0.1, 0, 0), shadow=True)
    plt.title('Procent ucieczek z miejsca zdarzenia')
    
    # 6. BEAT_TYPE - Typ patrolu
    plt.subplot(3, 2, 6)
    beat_type = df['beat_type'].value_counts().head(8)
    sns.barplot(x=beat_type.values, y=beat_type.index, palette='cubehelix')
    plt.title('Rozkład typów patroli (beat_type)')
    plt.xlabel('Liczba zdarzeń')
    
    plt.tight_layout()
    plt.savefig('additional_variables.png')
    plt.show()

    # Dodatkowe analizy statystyczne
    print("\nZaawansowane statystyki:")
    
    # Średnia ofiar wg hrabstwa
    print("\nŚrednia liczba ofiar wg hrabstwa:")
    print(df.groupby('county_location')['total_victims'].mean().sort_values(ascending=False).head())
    
    # Zależność oświetlenia od ciężkości kolizji
    print("\nZależność oświetlenia od ciężkości kolizji:")
    print(pd.crosstab(df['lighting'], df['collision_severity'], normalize='index'))
    
    # Współczynnik ucieczek wg typu patrolu
    print("\nWspółczynnik ucieczek wg typu patrolu:")
    print(df.groupby('beat_type')['hit_and_run'].value_counts(normalize=True).unstack().style.format('{:.1%}'))

# Uruchomienie analizy
if __name__ == "__main__":
    df = load_and_preprocess_data()  # Funkcja z poprzedniego kodu
    if df is not None:
        analyze_additional_variables(df)