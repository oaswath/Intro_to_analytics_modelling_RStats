import heapq
import random
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Fix the seed for reproducibility
random.seed(42)
np.random.seed(42)

class AirportSecuritySim:
    def __init__(self, arrival_rate, id_mean, scan_range, n_id, n_scan):
        self.arrival_rate = arrival_rate
        self.id_mean = id_mean
        self.scan_range = scan_range
        self.n_id = n_id
        self.n_scan = n_scan
        self.now = 0
        self.events = []
        self.id_busy = 0
        self.id_queue = []
        self.scan_busy = [0] * n_scan
        self.scan_queues = [[] for _ in range(n_scan)]
        self.passenger_log = {}

    def schedule(self, time, etype, data):
        heapq.heappush(self.events, (time, etype, data))

    def run(self, duration):
        # Initial arrival
        self.schedule(random.expovariate(self.arrival_rate), 'ARRIVAL', 0)
        p_counter = 0
        while self.events:
            time, etype, pid = heapq.heappop(self.events)
            if time > duration: break
            self.now = time
            
            if etype == 'ARRIVAL':
                p_counter += 1
                self.passenger_log[pid] = {'arrival': self.now}
                self.schedule(self.now + random.expovariate(self.arrival_rate), 'ARRIVAL', p_counter)
                
                # Stage 1: ID Check
                if self.id_busy < self.n_id:
                    self.id_busy += 1
                    self.passenger_log[pid]['start_id'] = self.now
                    self.schedule(self.now + random.expovariate(1.0/self.id_mean), 'END_ID', pid)
                else:
                    self.id_queue.append(pid)
                    
            elif etype == 'END_ID':
                self.passenger_log[pid]['end_id'] = self.now
                # Next from ID Queue
                if self.id_queue:
                    next_p = self.id_queue.pop(0)
                    self.passenger_log[next_p]['start_id'] = self.now
                    self.schedule(self.now + random.expovariate(1.0/self.id_mean), 'END_ID', next_p)
                else:
                    self.id_busy -= 1
                    
                # Stage 2: Scanner (Shortest Queue)
                q_loads = [len(self.scan_queues[i]) + self.scan_busy[i] for i in range(self.n_scan)]
                best_lane = np.argmin(q_loads)
                
                if self.scan_busy[best_lane] == 0:
                    self.scan_busy[best_lane] = 1
                    self.passenger_log[pid]['start_scan'] = self.now
                    self.schedule(self.now + random.uniform(*self.scan_range), 'END_SCAN', (pid, best_lane))
                else:
                    self.scan_queues[best_lane].append(pid)
                    
            elif etype == 'END_SCAN':
                pid, lane = pid
                self.passenger_log[pid]['end_scan'] = self.now
                # Next from Lane Queue
                if self.scan_queues[lane]:
                    next_p = self.scan_queues[lane].pop(0)
                    self.passenger_log[next_p]['start_scan'] = self.now
                    self.schedule(self.now + random.uniform(*self.scan_range), 'END_SCAN', (next_p, lane))
                else:
                    self.scan_busy[lane] = 0
        
        waits = [d['start_id'] - d['arrival'] + d['start_scan'] - d['end_id'] for d in self.passenger_log.values() if 'end_scan' in d]
        return np.mean(waits) if waits else float('inf')

# Simulation parameters
scenarios = [
    {'label': 'Moderate (Lambda=5)', 'lambda': 5, 'id_range': range(4, 10), 'scan_range': range(4, 10)},
    {'label': 'Busy (Lambda=50)', 'lambda': 50, 'id_range': range(38, 45), 'scan_range': range(38, 45)}
]

# Run Simulation Grid
results = []
for scen in scenarios:
    for n_id in scen['id_range']:
        for n_scan in scen['scan_range']:
            # Run longer to ensure stable results
            sim = AirportSecuritySim(scen['lambda'], 0.75, (0.5, 1.0), n_id, n_scan)
            avg_wait = sim.run(500)
            results.append({
                'Scenario': scen['label'],
                'Checkers': n_id,
                'Scanners': n_scan,
                'Avg_Wait': avg_wait
            })

df_res = pd.DataFrame(results)

# Generate plots separately
for scen_label in df_res['Scenario'].unique():
    subset = df_res[df_res['Scenario'] == scen_label]
    sanitized_label = scen_label.replace(" ", "_").replace("=", "").replace("(", "").replace(")", "").replace("λ", "L")
    
    # 1. Line Plot: Wait vs Scanners
    plt.figure(figsize=(10, 6))
    sns.lineplot(data=subset, x='Scanners', y='Avg_Wait', hue='Checkers', marker='o', palette='viridis')
    plt.axhline(y=15, color='red', linestyle='--', label='15m Threshold')
    plt.title(f"Average Wait Time vs. Scanner Count ({scen_label})")
    plt.xlabel("Number of Personal Scanners")
    plt.ylabel("Average Total Wait Time (min)")
    plt.legend(title="ID Checkers", loc='upper right')
    plt.grid(True, alpha=0.3)
    plt.savefig(f"wait_vs_scanners_{sanitized_label}.png")
    plt.close()
    
    # 2. Heatmap
    plt.figure(figsize=(10, 8))
    pivot = subset.pivot(index='Checkers', columns='Scanners', values='Avg_Wait')
    sns.heatmap(pivot, annot=True, fmt=".2f", cmap="YlOrRd")
    plt.title(f"Heatmap of Average Wait Times ({scen_label})")
    plt.xlabel("Number of Personal Scanners")
    plt.ylabel("Number of ID Checkers")
    plt.savefig(f"heatmap_{sanitized_label}.png")
    plt.close()

# Output CSV for user
df_res.to_csv("simulation_grid_results.csv", index=False)
print("Simulation complete. Plots and CSV generated.")