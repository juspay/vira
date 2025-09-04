import { test, expect } from '@playwright/test';

test('add repository and verify on repositories page', async ({ page }) => {
  // Navigate to Vira main page
  await page.goto('/');
  
  // Navigate to repositories page
  await page.getByRole('link', { name: /repositories/i }).click();
  
  // Fill the repository name field (using semantic label)
  const nameInput = page.getByLabel('Repository Name');
  await nameInput.fill('haskell-template');
  
  // Fill the Git Clone URL field (using semantic label)
  const urlInput = page.getByLabel('Git Clone URL');
  await urlInput.fill('https://github.com/srid/haskell-template');
  
  // Click the Add Repository button
  await page.getByRole('button', { name: /add repository/i }).click();
  
  // Reload the repositories page to verify the repository was added
  await page.reload();
  
  // Verify the haskell-template repository appears on the page
  await expect(page.getByRole('link', { name: /haskell-template/ })).toBeVisible();
});