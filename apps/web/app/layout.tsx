import './globals.css';

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    // suppressHydrationWarning — дополнительная защита от «шумных» расширений
    <html lang="ru" suppressHydrationWarning>
      <body>{children}</body>
    </html>
  );
}
