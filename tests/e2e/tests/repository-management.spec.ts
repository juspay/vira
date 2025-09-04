import { test, expect } from '@playwright/test';

test('add repository and verify on repositories page', async ({ page }) => {
  // Navigate to Vira main page
  await page.goto('/');
  
  // Navigate to repositories page
  await page.getByRole('link', { name: /repositories/i }).click();
  
  // Fill the repository name field (use placeholder text)
  const nameInput = page.getByPlaceholder(/my-awesome-project/);
  await nameInput.fill('haskell-template');
  
  // Fill the Git Clone URL field (use placeholder text)
  const urlInput = page.getByPlaceholder(/https:\/\/github\.com\/user\/repo\.git/);
  await urlInput.fill('https://github.com/srid/haskell-template');
  
  // Click the Add Repository button
  await page.getByRole('button', { name: /add repository/i }).click();
  
  // Reload the repositories page to verify the repository was added
  await page.reload();
  
  // Verify the haskell-template repository appears on the page
  await expect(page.getByRole('link', { name: /haskell-template/ })).toBeVisible();
});